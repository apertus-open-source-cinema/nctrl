// SPDX-FileCopyrightText: © 2020 Robin Ole Heinemann <robin.ole.heinemann@gmail.com>
// SPDX-License-Identifier: AGPL-3.0-only

#[cfg(test)]
mod tests {
    use std::{fs::File, io::Read, path::PathBuf};

    use crate::{
        camera::{camera, globals, set_camera, with_camera, Camera, SharedCamera},
        device::DeviceLike,
        fuseable_fs::FuseableFS,
        serde_util::FILE_OPENER,
        value::{Bytes, Value},
    };

    #[test]
    fn test_parse_beta() {
        let path = "camera_descriptions/beta/beta.yml";
        let mut f = File::open(path).unwrap();
        FILE_OPENER.set_path(PathBuf::from(path));

        let mut contents = String::new();
        f.read_to_string(&mut contents).unwrap();

        let mut cam: Camera = serde_yaml::from_str(&contents).unwrap();

        cam.mocked(true);

        set_camera(cam);
    }

    #[test]
    fn test_parse_micro_r2() {
        let path = "camera_descriptions/micro_r2/micro_r2.yml";
        let mut f = File::open(path).unwrap();
        FILE_OPENER.set_path(PathBuf::from(path));

        let mut contents = String::new();
        f.read_to_string(&mut contents).unwrap();

        let mut cam: Camera = serde_yaml::from_str(&contents).unwrap();

        cam.mocked(true);

        set_camera(cam);
    }

    fn cam() {
        let path = "camera_descriptions/test/test.yml";
        let mut f = File::open(path).unwrap();
        FILE_OPENER.set_path(PathBuf::from(path));

        let mut contents = String::new();
        f.read_to_string(&mut contents).unwrap();

        let mut cam: Camera = serde_yaml::from_str(&contents).unwrap();

        cam.mocked(true);

        set_camera(cam);
    }

    macro_rules! eq {
        ($a:expr, $b:expr) => {
            assert_eq!($a, $b)
        };
    }

    macro_rules! test_cam {
        ($name:ident, |$cam:ident| $test:tt) => {
            #[test]
            fn $name() {
                cam();

                with_camera(|$cam| $test);
            }
        };
    }

    macro_rules! test_device {
        ($name:ident, $dev:ident, |$device:ident| $test:tt) => {
            #[test]
            fn $name() {
                cam();

                with_camera(|cam| {
                    let $device = cam.devices[stringify!($dev)].lock().unwrap();
                    $test
                });
            }
        };
    }

    test_cam!(model, |cam| { eq!(cam.camera_model, "test_model") });

    test_cam!(globals_from_rust, |_cam| {
        eq!(globals("test_uint").unwrap().into::<u64>().unwrap(), 1234);
        eq!(globals("test_int").unwrap().into::<i64>().unwrap(), -1234);
        eq!(globals("test_float").unwrap().into::<f64>().unwrap(), 3.1415);
        eq!(globals("test_string").unwrap().into::<String>().unwrap(), "testtesttest");
    });

    test_device!(test_mmap_raw_read, test_mmap, |dev| {
        eq!(dev.read_raw("test_mmap_raw_1").unwrap(), Value::Bytes(vec![0xe4, 0x33]));
        eq!(dev.read_raw("test_mmap_raw_1").unwrap().into::<i64>().unwrap(), -0x1bcd);
        eq!(dev.read_raw("test_mmap_raw_2").unwrap(), Value::Bytes(vec![0x12, 0x34, 0x56, 0x78]));
        eq!(dev.read_raw("test_mmap_raw_2").unwrap().into::<i64>().unwrap(), 0x12345678);
        eq!(dev.read_raw("test_mmap_raw_2").unwrap().into::<u64>().unwrap(), 0x12345678);
    });

    test_device!(test_mmap_raw_read_write, test_mmap, |dev| {
        dev.write_raw("test_mmap_raw_1", Value::Bytes(vec![0x12, 0x34])).unwrap();
        eq!(dev.read_raw("test_mmap_raw_1").unwrap(), Value::Bytes(vec![0x12, 0x34]));
        eq!(dev.read_raw("test_mmap_raw_1").unwrap().into::<i64>().unwrap(), 0x1234);

        dev.write_raw("test_mmap_raw_1", Value::Int(-0x4321)).unwrap();
        eq!(dev.read_raw("test_mmap_raw_1").unwrap().into::<i64>().unwrap(), -0x4321);

        dev.write_raw("test_mmap_raw_1", Value::UInt(0x1234)).unwrap();
        eq!(dev.read_raw("test_mmap_raw_1").unwrap().into::<u64>().unwrap(), 0x1234);

        dev.write_raw("test_mmap_raw_1", Value::String("0xc0de".to_owned())).unwrap();
        eq!(dev.read_raw("test_mmap_raw_1").unwrap().into::<i64>().unwrap(), 0xc0de);
        eq!(dev.read_raw("test_mmap_raw_1").unwrap().into::<u64>().unwrap(), 0xc0de);

        eq!(dev.read_raw("test_mmap_raw_2").unwrap(), Value::Bytes(vec![0x12, 0x34, 0x56, 0x78]));
        eq!(dev.read_raw("test_mmap_raw_2").unwrap().into::<i64>().unwrap(), 0x12345678);
        eq!(dev.read_raw("test_mmap_raw_2").unwrap().into::<u64>().unwrap(), 0x12345678);
    });

    /*
    test_device_eq!(test_mmap_raw_1_int, test_mmap,
             |dev| {
                 dev.read_raw("test_mmap_raw_1").unwrap().into::<i64>().unwrap()
             },
             -0x1bcd
    );

    test_device_eq!(test_mmap_raw_2, test_mmap,
             |dev| {
                 dev.read_raw("test_mmap_raw_2").unwrap()
             },
             Value::Bytes(vec![0x12, 0x34, 0x56, 0x78])
    );

    test_device_eq!(test_mmap_raw_2_int, test_mmap,
             |dev| {
                 dev.read_raw("test_mmap_raw_2").unwrap().into::<u64>().unwrap()
             },
             0x12345678
    );

    test_device_eq!(test_mmap_raw_2_int, test_mmap,
             |dev| {
                 dev.read_raw("test_mmap_raw_2").unwrap().into::<u64>().unwrap()
             },
             0x12345678
    );
    */
}
