macro_rules! sound_test {
    (
        $(
            $( #[$attr:meta] )*
            $name:ident => $kind:path,
        )*
    ) => {
        $(
            #[test]
            $( #[$attr] )*
            fn $name() {
                let cache = assets_manager::AssetCache::new("../../assets").expect("oops");
                assert!(cache.load::<$kind>("test.sounds.silence").is_ok());
            }
        )*
    };
}

sound_test! {
    #[cfg(feature ="flac")]
    test_flac => crate::Flac,

    #[cfg(feature ="mp3")]
    // #[ignore = "Blocks on https://github.com/RustAudio/rodio/issues/411"]
    test_mp3 => crate::Mp3,

    #[cfg(feature ="vorbis")]
    test_vorbis => crate::Vorbis,

    #[cfg(feature ="wav")]
    test_wav => crate::Wav,
}
