use super::DirEntry;
use crate::{utils::IdBuilder, SharedString};
use std::{
    collections::HashMap,
    fmt, hash, io,
    path::{self, Path},
};
use sync_file::SyncFile;

#[cfg(doc)]
use super::Source;

#[derive(Clone, Hash, PartialEq, Eq)]
struct FileDesc(SharedString, SharedString);

impl fmt::Debug for FileDesc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("FileDesc")
            .field("id", &self.0)
            .field("ext", &self.1)
            .finish()
    }
}

/// This hack enables us to use a `(&str, &str)` as a key for an HashMap without
/// allocating a `FileDesc`
trait FileKey {
    fn id(&self) -> &str;
    fn ext(&self) -> &str;
}

impl FileKey for FileDesc {
    fn id(&self) -> &str {
        &self.0
    }

    fn ext(&self) -> &str {
        &self.1
    }
}

impl FileKey for (&'_ str, &'_ str) {
    fn id(&self) -> &str {
        self.0
    }

    fn ext(&self) -> &str {
        self.1
    }
}

impl<'a> std::borrow::Borrow<dyn FileKey + 'a> for FileDesc {
    fn borrow(&self) -> &(dyn FileKey + 'a) {
        self
    }
}

impl PartialEq for dyn FileKey + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id() && self.ext() == other.ext()
    }
}

impl Eq for dyn FileKey + '_ {}

impl hash::Hash for dyn FileKey + '_ {
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        self.id().hash(hasher);
        self.ext().hash(hasher);
    }
}

/// An entry in a archive directory.
#[derive(Debug)]
enum OwnedEntry {
    File(FileDesc),
    Dir(SharedString),
}

impl OwnedEntry {
    fn as_dir_entry(&self) -> DirEntry {
        match self {
            OwnedEntry::File(FileDesc(id, ext)) => DirEntry::File(id, ext),
            OwnedEntry::Dir(id) => DirEntry::Directory(id),
        }
    }
}

/// Register a file of an archive in maps.
fn register_file(
    file: tar::Entry<'_, impl io::Read>,
    files: &mut HashMap<FileDesc, (u64, u64)>,
    dirs: &mut HashMap<SharedString, Vec<OwnedEntry>>,
    id_builder: &mut IdBuilder,
) {
    id_builder.reset();

    let typ = file.header().entry_type();
    match typ {
        tar::EntryType::Regular | tar::EntryType::Directory => (),
        tar::EntryType::Link
        | tar::EntryType::Symlink
        | tar::EntryType::Char
        | tar::EntryType::Block
        | tar::EntryType::Fifo
        | tar::EntryType::GNUSparse => {
            log::warn!("Unsupported file type: {typ:?}");
            return;
        }
        _ => log::warn!("Unexpected entry type: {typ:?}"),
    }

    let Ok(path) = file.path() else {
        log::warn!("Unsupported path in tar archive");
        return;
    };

    // Parse the path and register it.
    // The closure is used as a cheap `try` block.
    let ok = (|| {
        // Fill `id_builder` from the parent's components
        for comp in path.parent()?.components() {
            match comp {
                path::Component::Normal(s) => id_builder.push(s.to_str()?)?,
                path::Component::ParentDir => id_builder.pop()?,
                path::Component::CurDir => continue,
                _ => return None,
            }
        }

        // Build the ids of the file and its parent.
        let parent_id = id_builder.join();
        id_builder.push(path.file_stem()?.to_str()?)?;
        let id = id_builder.join();

        // Register the file in the maps.
        let entry = if file.header().entry_type().is_file() {
            let ext = crate::utils::extension_of(&path)?.into();
            let desc = FileDesc(id, ext);

            let start = file.raw_file_position();
            let size = file.size();

            files.insert(desc.clone(), (start, size));
            OwnedEntry::File(desc)
        } else {
            if !dirs.contains_key(&id) {
                dirs.insert(id.clone(), Vec::new());
            }
            OwnedEntry::Dir(id)
        };
        dirs.entry(parent_id).or_default().push(entry);

        Some(())
    })()
    .is_some();

    if !ok {
        log::warn!("Unsupported path in tar archive: {path:?}");
    }
}

/// A [`Source`] to load assets from a tar archive.
///
/// The archive can be backed by any reader that also implements [`io::Seek`]
/// and [`Clone`] or by a byte slice. In the second case, reading files will
/// not involve copying data.
pub struct Tar<R = SyncFile> {
    inner: UntypedTar,
    _typ: std::marker::PhantomData<R>,
}

impl Tar<SyncFile> {
    /// Creates a `Zip` archive backed by the file at the given path.
    #[inline]
    pub fn open<P: AsRef<std::path::Path>>(path: P) -> io::Result<Self> {
        Self::_open(path.as_ref())
    }

    fn _open(path: &Path) -> io::Result<Self> {
        let file = SyncFile::open(path)?;
        let label = path.display().to_string();
        Self::from_reader_with_label(file, label)
    }
}

impl<T: AsRef<[u8]>> Tar<io::Cursor<T>> {
    /// Creates a `Tar` archive backed by a byte buffer in memory.
    ///
    /// Reading a file from this archive will not copy its content.
    #[inline]
    pub fn from_bytes(bytes: T) -> io::Result<Self> {
        Self::new(io::Cursor::new(bytes), Bytes, None)
    }

    /// Creates a `Tar` archive backed by a byte buffer in memory.
    ///
    /// Reading a file from this archive will not copy its content.
    ///
    /// An additionnal label that will be used in errors can be added.
    #[inline]
    pub fn from_bytes_with_label(bytes: T, label: String) -> io::Result<Self> {
        Self::new(io::Cursor::new(bytes), Bytes, Some(label))
    }
}

impl<R> Tar<R>
where
    R: io::Read + io::Seek + Clone,
{
    /// Creates a `Tar` archive backed by a reader that supports seeking.
    ///
    /// **Warning**: This will clone the reader each time it is read, so you should
    /// ensure that is cheap to clone.
    pub fn from_reader(reader: R) -> io::Result<Self> {
        Self::new(reader, Reader, None)
    }

    /// Creates a `Tar` archive backed by a reader that supports seeking.
    ///
    /// An additionnal label that will be used in errors can be added.
    ///
    /// **Warning**: This will clone the reader each time it is read, so you should
    /// ensure that is cheap to clone.
    pub fn from_reader_with_label(reader: R, label: String) -> io::Result<Self> {
        Self::new(reader, Reader, Some(label))
    }
}

impl<R> Tar<R> {
    fn new<F, T>(reader: R, f: F, label: Option<String>) -> io::Result<Self>
    where
        F: FnOnce(R) -> T,
        T: TarReader,
    {
        Ok(Tar {
            inner: UntypedTar::new(Box::new(f(reader)), label)?,
            _typ: std::marker::PhantomData,
        })
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "tar")))]
impl<R> super::Source for Tar<R> {
    #[inline]
    fn read(&self, id: &str, ext: &str) -> io::Result<super::FileContent> {
        self.inner.read(id, ext)
    }

    #[inline]
    fn read_dir(&self, id: &str, f: &mut dyn FnMut(DirEntry)) -> io::Result<()> {
        self.inner.read_dir(id, f)
    }

    #[inline]
    fn exists(&self, entry: DirEntry) -> bool {
        self.inner.exists(entry)
    }
}

impl<R> fmt::Debug for Tar<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

struct UntypedTar {
    // Unsafe container without any lifetime marker nor Send/Sync bounds.
    // They are added by the `PhantomData` field in `Tar`.
    reader: AllAutoTraits<Box<dyn TarReader>>,

    files: HashMap<FileDesc, (u64, u64)>,
    dirs: HashMap<SharedString, Vec<OwnedEntry>>,
    label: Option<String>,
}

impl UntypedTar {
    fn new(mut reader: Box<dyn TarReader + '_>, label: Option<String>) -> io::Result<Self> {
        let mut archive = tar::Archive::new(reader.read_seeker());
        let mut id_builder = IdBuilder::default();

        let mut files = HashMap::new();
        let mut dirs = HashMap::new();

        for file in archive.entries_with_seek()? {
            register_file(file?, &mut files, &mut dirs, &mut id_builder)
        }

        let reader = unsafe { std::mem::transmute(reader) };

        Ok(UntypedTar {
            reader: AllAutoTraits(reader),
            files,
            dirs,
            label,
        })
    }
}

#[cfg_attr(docsrs, doc(cfg(feature = "tar")))]
impl super::Source for UntypedTar {
    fn read(&self, id: &str, ext: &str) -> io::Result<super::FileContent> {
        let &(start, size) = self
            .files
            .get(&(id, ext) as &dyn FileKey)
            .ok_or_else(|| error::find_file(id, &self.label))?;

        self.reader
            .0
            .read_file(start, size as usize)
            .map_err(|err| error::read_file(err, id, &self.label))
    }

    fn read_dir(&self, id: &str, f: &mut dyn FnMut(DirEntry)) -> io::Result<()> {
        let dir = self
            .dirs
            .get(id)
            .ok_or_else(|| error::find_dir(id, &self.label))?;
        dir.iter().map(OwnedEntry::as_dir_entry).for_each(f);
        Ok(())
    }

    fn exists(&self, entry: DirEntry) -> bool {
        match entry {
            DirEntry::File(id, ext) => self.files.contains_key(&(id, ext) as &dyn FileKey),
            DirEntry::Directory(id) => self.dirs.contains_key(id),
        }
    }
}

impl fmt::Debug for UntypedTar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Tar")
            .field("label", &self.label)
            .field("dirs", &self.dirs)
            .finish()
    }
}

struct AllAutoTraits<T>(T);

unsafe impl<T> Send for AllAutoTraits<T> {}
unsafe impl<T> Sync for AllAutoTraits<T> {}
impl<T> std::panic::UnwindSafe for AllAutoTraits<T> {}
impl<T> std::panic::RefUnwindSafe for AllAutoTraits<T> {}

trait ReadSeek: io::Read + io::Seek {}
impl<R: io::Read + io::Seek> ReadSeek for R {}

trait TarReader {
    fn read_file(&self, start: u64, size: usize) -> io::Result<super::FileContent<'_>>;

    fn read_seeker(&mut self) -> &mut dyn ReadSeek;
}

struct Reader<R>(R);
struct Bytes<T>(io::Cursor<T>);

impl<R: io::Read + io::Seek + Clone> TarReader for Reader<R> {
    fn read_file(&self, start: u64, size: usize) -> io::Result<super::FileContent<'_>> {
        let mut reader = self.0.clone();

        let mut buf = vec![0; size];
        reader.seek(io::SeekFrom::Start(start))?;
        reader.read_exact(&mut buf)?;

        Ok(super::FileContent::Buffer(buf))
    }

    fn read_seeker(&mut self) -> &mut dyn ReadSeek {
        &mut self.0
    }
}

impl<T: AsRef<[u8]>> TarReader for Bytes<T> {
    fn read_file(&self, start: u64, size: usize) -> io::Result<super::FileContent<'_>> {
        let start = start as usize;
        let tar = self.0.get_ref().as_ref();
        let file = tar
            .get(start..start + size)
            .ok_or(io::ErrorKind::InvalidData)?;
        Ok(super::FileContent::Slice(file))
    }

    fn read_seeker(&mut self) -> &mut dyn ReadSeek {
        &mut self.0
    }
}

mod error {
    use std::{fmt, io};

    #[cold]
    pub fn find_file(id: &str, label: &Option<String>) -> io::Error {
        let msg = match label {
            Some(lbl) => format!("Could not find asset \"{id}\" in {lbl}"),
            None => format!("Could not find asset \"{id}\" in TAR"),
        };

        io::Error::new(io::ErrorKind::NotFound, msg)
    }

    #[cold]
    pub fn read_file(err: io::Error, id: &str, label: &Option<String>) -> io::Error {
        #[derive(Debug)]
        struct Error {
            err: io::Error,
            msg: String,
        }
        impl fmt::Display for Error {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(&self.msg)
            }
        }
        impl std::error::Error for Error {
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
                Some(&self.err)
            }
        }

        let msg = match label {
            Some(lbl) => format!("Could not read \"{id}\" in {lbl}"),
            None => format!("Could not read \"{id}\" in TAR"),
        };

        io::Error::new(err.kind(), Error { err, msg })
    }

    #[cold]
    pub fn find_dir(id: &str, label: &Option<String>) -> io::Error {
        let msg = match label {
            Some(lbl) => format!("Could not find directory \"{id}\" in {lbl}"),
            None => format!("Could not find directory \"{id}\" in TAR"),
        };

        io::Error::new(io::ErrorKind::NotFound, msg)
    }
}
