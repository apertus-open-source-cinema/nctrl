use std::fmt::Debug;

use fuseable::Fuseable;
use rlua::RegistryKey;

use crate::camera::Camera;


// also think about how the arguments should be typed
// we probably want everything to travel as Vec<u8> and only be casted to the wanted type when
// the used
// however: how do we distinguish numbers in binary from numbers in ascii
// maybe we just say you are never supposed to do numbers in binary and only binary data as binary, in which case you use it as Vec<u8>? That sounds good (of course it has some overhead, especially when considering transports where we easily could use binary data)
// so then we can finally easily strip and add \n's for non binary data

// or userdata? yes investigate userdata!!!
pub trait DeviceLike {
    read_{raw, cooked, computed}
    write_{raw, cooked, computed}
}

pub trait Script: Debug + Fuseable {
    fn run(&self, devices: HashMap<String, DeviceLike>, args: HashMap<String, String>) -> fuseable::Result<String>;

    // the devices this script needs
    fn devices(&self) -> Vec<String>;

    /*
    fn read(&self, cam: &Camera) -> fuseable::Result<String>;
    fn write(&self, cam: &Camera, value: Vec<u8>) -> fuseable::Result<()>;

    fn read_key(&self) -> Option<&RegistryKey>;
    fn write_key(&self) -> Option<&RegistryKey>;

    fn init_functions(&mut self, lua_vm: &rlua::Lua);
    */
}

macro_rules! script {
    { $desc:tt $struct_name:ident {$($elem:ident:$elem_typ:ty),*}  => {
        read => ($self_read:ident $(,$regs_read:ident)*) $body_read:block
        write [$value_name:ident] => ($self_write:ident $(,$regs_write:ident)*) $body_write:block
    } } => {
        paste::item!{
            #[derive(Debug, Fuseable, Default)]
            struct [<$struct_name Args>] {
                $($elem: $elem_typ,)*
            }

            #[derive(Debug, Fuseable)]
            struct $struct_name {
                description: String,
                #[fuseable(skip)]
                read_function: Option<rlua::RegistryKey>,
                #[fuseable(skip)]
                write_function: Option<rlua::RegistryKey>,
                args: [<$struct_name Args>]
            }
        }

            impl Default for $struct_name {
                fn default() -> $struct_name {
                    $struct_name {
                        description: $desc.to_string(),
                        read_function: None,
                        write_function: None,
                        args: Default::default()
                    }
                }
            }


            impl super::Script for $struct_name {
                fn read_key(&self) -> Option<&rlua::RegistryKey> {
                    self.read_function.as_ref()
                }

                fn write_key(&self) -> Option<&rlua::RegistryKey> {
                    self.write_function.as_ref()
                }

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

                #[allow(unused_variables)]
                fn init_functions(&mut $self_read, lua_vm: &rlua::Lua) {
                    lua_vm.context(|ctx| {
                        // closure capture is shit
                        let read_function = &mut $self_read.read_function;
                        let write_function = &mut $self_read.write_function;

                        let read_func = ctx.create_function::<_, String, _>(|_, devices: rlua::Table| {
                            $(let $regs_read = crate::lua_util::LuaDeviceWrapper(devices.get(stringify!($regs_read))?);)*

                            $body_read.map_err(crate::lua_util::FailureCompat::failure_to_lua)
                        }).unwrap();

                        *read_function = Some(ctx.create_registry_value(read_func).unwrap());

                        let write_func = ctx.create_function::<_, (), _>(|_, (devices, $value_name): (rlua::Table, String)| {
                            // TODO(robin): find a better way to pass a Vec<u8>? (maybe userdata?)
                            let $value_name = $value_name.as_bytes().to_vec();
                            $(let $regs_write = crate::lua_util::LuaDeviceWrapper(devices.get(stringify!($regs_write))?);)*

                            $body_write.map_err(crate::lua_util::FailureCompat::failure_to_lua)
                        }).unwrap();

                        *write_function = Some(ctx.create_registry_value(write_func).unwrap());
                    })
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
