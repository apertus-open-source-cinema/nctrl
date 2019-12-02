use std::fmt::Debug;

use fuseable::Fuseable;

use crate::camera::Camera;

pub trait Script: Debug + Fuseable {
    fn read(&self, cam: &Camera) -> fuseable::Result<String>;
    fn write(&self, cam: &Camera, value: Vec<u8>) -> fuseable::Result<()>;
}

macro_rules! script {
    { $desc:tt $struct_name:ident {$($elem:ident:$elem_typ:ty),*}  => {
        read => ($self_read:ident $(,$regs_read:ident)*) $body_read:block
        write [$value_name:ident] => ($self_write:ident $(,$regs_write:ident)*) $body_write:block
    } } => {
            #[derive(Debug, Fuseable)]
            struct $struct_name {
                description: String,
                $($elem: $elem_typ,)*
            }

            impl Default for $struct_name {
                fn default() -> $struct_name {
                    #[derive(Default)]
                    struct ForDefault {
                        $($elem: $elem_typ,)*
                    }

                    #[allow(unused_variables)]
                    let for_default = ForDefault::default();

                    $struct_name {
                        description: $desc.to_string(),
                        $($elem: for_default.$elem),*
                    }
                }
            }


            impl super::Script for $struct_name {
                #[allow(unused_variables)]
                fn read(&$self_read, cam: &super::Camera) -> fuseable::Result<String> {
                    $(let $regs_read = cam.devices[stringify!($regs_read)].lock().unwrap();)*

                    $body_read
                }
                #[allow(unused_variables)]
                fn write(&$self_write, cam: &super::Camera, $value_name: Vec<u8>) -> fuseable::Result<()> {
                    $(let $regs_write = cam.devices[stringify!($regs_write)].lock().unwrap();)*

                    $body_write
                }
            }
    };
}

macro_rules! script_set {
    { $set_name:ident => { $($name:tt:$script:ident),* } } => {
        pub struct $set_name {}

        impl $set_name {
            pub fn get_scripts() -> std::collections::HashMap<String, Box<dyn super::Script>> {
                #[allow(unused_mut)]
                let mut map = std::collections::HashMap::new();

                $(map.insert($name.to_owned(), Box::new($script::default()) as Box<dyn super::Script>);)*

                map
            }
        }
    };
}

macro_rules! script_config {
    ( $($script:ident => $tag:tt),* ) => {
        pub fn scripts_from_model(model: &str) -> std::collections::HashMap<String, Box<dyn Script>> {
            match model {
                $(
                    $tag => $script::get_scripts(),
                )*
                _ => {
                        panic!("unsupported model {}", model);
                }
            }
        }
    }
}

mod beta;
mod micro_r2;

use beta::BetaScripts;
use micro_r2::MicroR2Scripts;

script_config!(MicroR2Scripts => "micro-r2", BetaScripts => "beta");
