// SPDX-FileCopyrightText: © 2019 Jaro Habiger <jarohabiger@googlemail.com>
// SPDX-FileCopyrightText: © 2019 Robin Ole Heinemann <robin.ole.heinemann@gmail.com>
// SPDX-License-Identifier: AGPL-3.0-only

use env_logger::{Builder, Env};
use log::{error, info};
use nctrl::{
    camera::{camera, set_camera, with_camera, Camera, SharedCamera},
    fuseable_fs::FuseableFS,
    serde_util::FILE_OPENER,
};
use std::{ffi::OsStr, io::Read, path::PathBuf};
use structopt::StructOpt;

// TODO(robin): consider making Fusable work with Value?
// TODO(robin): rework the Error handling (with own error type per component)


/// Basic daemon for controlling the various components of a camera
#[derive(StructOpt, Debug)]
#[structopt(name = "ctrl")]
struct Opt {
    /// Config file describing the camera components and their functionality
    #[structopt(name = "FILE")]
    file: String,
    /// Set all communication channels to mock mode to prevent them from
    /// actually doing anything
    #[structopt(short = "m", long = "mock")]
    mock: bool,
    /// Mountpoint of the fuse config filesystem
    #[structopt(short = "d", long = "mountpoint", default_value = ".propfs")]
    mountpoint: String,
}

fn main() {
    Builder::from_env(Env::default().default_filter_or("info"))
        .format_indent(Some(4))
        .format_timestamp(None)
        .init();

    let opt = Opt::from_args();

    let mut f = match FILE_OPENER.open(&opt.file) {
        Ok(f) => f,
        Err(e) => {
            error!("could not open camera descripton {}: {}", opt.file, e);
            std::process::exit(1);
        }
    };

    FILE_OPENER.set_path(PathBuf::from(opt.file));

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong reading the file");

    info!("parsing yml file");
    let mut cam: Camera = serde_yaml::from_str(&contents).unwrap();

    info!("setting mocked mode to {}", opt.mock);
    cam.mocked(opt.mock);

    set_camera(cam);

    // initialize stuff, for example run the init script
    with_camera(|cam| cam.init()).unwrap();

    let options = ["-o", "allow_other", "-o", "rw", "-o", "fsname=propfs", "-o", "auto_unmount"]
        .iter()
        .map(|o| o.as_ref())
        .collect::<Vec<&OsStr>>();

    info!("successfully initialized");

    fuse::mount(FuseableFS::new(&mut SharedCamera { camera: camera() }), opt.mountpoint, &options)
        .unwrap();
}
