use assets_manager::{loader, Asset, BoxedError, SharedBytes};
use rodio::{decoder::DecoderError, Decoder};
use std::{borrow::Cow, io};

#[cfg(test)]
mod tests;

pub struct RodioLoader;

macro_rules! sound_assets {
    (
        $(
            #[doc = $doc:literal]
            #[cfg( $( $cfg:tt )* )]
            struct $name:ident => (
                $decoder:path,
                [$($ext:literal),*],
            );
        )*
    ) => {
        $(
            #[doc = $doc]
            #[cfg($($cfg)*)]
            #[cfg_attr(docsrs, doc(cfg($($cfg)*)))]
            #[derive(Clone, Debug)]
            pub struct $name(SharedBytes);

            #[cfg($($cfg)*)]
            #[cfg_attr(docsrs, doc(cfg($($cfg)*)))]
            impl loader::Loader<$name> for RodioLoader {
                #[inline]
                fn load(content: Cow<[u8]>, _: &str) -> Result<$name, BoxedError> {
                    let bytes = content.into();
                    Ok($name::new(bytes)?)
                }
            }

            #[cfg($($cfg)*)]
            #[cfg_attr(docsrs, doc(cfg($($cfg)*)))]
            impl Asset for $name {
                const EXTENSIONS: &'static [&'static str] = &[$( $ext ),*];
                type Loader = RodioLoader;
            }

            #[cfg($($cfg)*)]
            impl $name {
                /// Creates a new sound from raw bytes.
                #[inline]
                pub fn new(bytes: SharedBytes) -> Result<$name, DecoderError> {
                    // We have to clone the bytes here because `Decoder::new`
                    // requires a 'static lifetime, but it should be cheap
                    // anyway.
                    let _ = $decoder(io::Cursor::new(bytes.clone()))?;
                    Ok($name(bytes))
                }

                /// Creates a [`Decoder`] that can be send to `rodio` to play
                /// sounds.
                #[inline]
                pub fn decoder(self) -> Decoder<io::Cursor<SharedBytes>> {
                    $decoder(io::Cursor::new(self.0)).unwrap()
                }

                #[inline]
                /// Returns a bytes slice of the sound content.
                pub fn as_bytes(&self) -> &[u8] {
                    &self.0
                }

                /// Convert the sound back to raw bytes.
                #[inline]
                pub fn into_bytes(self) -> SharedBytes {
                    self.0
                }
            }

            #[cfg($($cfg)*)]
            impl AsRef<[u8]> for $name {
                fn as_ref(&self) -> &[u8] {
                    &self.0
                }
            }
        )*
    }
}

sound_assets! {
    /// Load FLAC sounds
    #[cfg(any(feature = "flac", feature = "symphonia-flac"))]
    struct Flac => (
        Decoder::new_flac,
        ["flac"],
    );

    /// Load MP3 sounds
    #[cfg(any(feature = "mp3", feature = "symphonia-mp3"))]
    struct Mp3 => (
        Decoder::new_mp3,
        ["mp3"],
    );

    /// Load Vorbis sounds
    #[cfg(all(feature = "vorbis", not(feature = "symphonia-vorbis")))]
    struct Vorbis => (
        Decoder::new_vorbis,
        ["ogg"],
    );

    /// Load Vorbis sounds
    #[cfg(feature = "symphonia-vorbis")]
    struct Vorbis => (
        Decoder::new,
        ["ogg"],
    );

    /// Load WAV sounds
    #[cfg(any(feature = "wav", feature = "symphonia-wav"))]
    struct Wav => (
        Decoder::new_wav,
        ["wav"],
    );
}
