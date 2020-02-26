docker run \
    -t \
    -h "axiom-build" \
    -v $(pwd):/root/nctrl/ \
    -v $HOME/.cargo:/root/cargo \
    -w /root/nctrl/ \
    -l axiom-build \
    --env COLUMNS=$COLUMNS --env LINES=$LINES \
    --env CROSS_COMPILE=arm-buildroot-linux-musleabihf- \
    --env CFLAGS="-mfpu=neon" \
    --env FUSE_CROSS_STATIC_PATH=./thirdparty/ \
    --env FUSE_CROSS_STATIC_LIB=fuse \
    --env CARGO_HOME=/root/cargo \
    apertushq/axiom_build:latest \
    cargo build --release --target=armv7-unknown-linux-musleabihf
