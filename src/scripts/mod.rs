use fuseable::Fuseable;
use std::{collections::HashMap, fmt::Debug};

use crate::{
    device::DeviceLike,
    value::{ToValue, Value},
};

mod lua_script;
pub use crate::scripts::lua_script::LuaScript;


// also think about how the arguments should be typed
// we probably want everything to travel as Vec<u8> and only be casted to the
// wanted type when the used
// however: how do we distinguish numbers in binary from numbers in ascii
// maybe we just say you are never supposed to do numbers in binary and only
// binary data as binary, in which case you use it as Vec<u8>? That sounds good
// (of course it has some overhead, especially when considering transports where
// we easily could use binary data) so then we can finally easily strip and add
// \n's for non binary data

struct DeviceLikeWrapper<'a>(&'a dyn DeviceLike);

#[allow(dead_code)]
impl<'a> DeviceLikeWrapper<'a> {
    fn read_raw(&self, name: &str) -> fuseable::Result<Value> { self.0.read_raw(name) }

    fn write_raw<T: ToValue>(&self, name: &str, value: T) -> fuseable::Result<()> {
        self.0.write_raw(name, value.to_value()?)
    }

    fn read_cooked(&self, name: &str) -> fuseable::Result<Value> { self.0.read_cooked(name) }

    fn write_cooked<T: ToValue>(&self, name: &str, value: T) -> fuseable::Result<()> {
        self.0.write_cooked(name, value.to_value()?)
    }

    fn read_computed(&self, name: &str) -> fuseable::Result<Value> { self.0.read_computed(name) }

    fn write_computed<T: ToValue>(&self, name: &str, value: T) -> fuseable::Result<()> {
        self.0.write_computed(name, value.to_value()?)
    }
}

pub trait Script: Debug + Fuseable {
    fn run(
        &self,
        devices: HashMap<String, &dyn DeviceLike>,
        args: HashMap<String, Value>,
    ) -> fuseable::Result<Value>;

    // the devices this script needs
    fn devices(&self) -> Vec<String>;
}

macro_rules! script {
    { $desc:tt $struct_name:ident {$($args:ident:$args_ty:ty),*}  => {
        ($self:ident, $devices_name:ident = { $($devices:ident),* }) $body:block
    } } => {
        paste::item!{
            #[derive(Debug, Fuseable, Default)]
            struct [<$struct_name Args>] {
                $($args: $args_ty,)*
            }

            #[derive(Debug, Fuseable)]
            struct $struct_name {
                description: String,
                args: [<$struct_name Args>]
            }
        }

            impl Default for $struct_name {
                fn default() -> $struct_name {
                    $struct_name {
                        description: $desc.to_string(),
                        args: Default::default()
                    }
                }
            }


            impl super::Script for $struct_name {
                #[allow(unused_variables)]
                fn run(&$self, $devices_name: std::collections::HashMap<String, &dyn crate::device::DeviceLike>, args: std::collections::HashMap<String, Value>) -> fuseable::Result<Value> {
                    $(let $devices = crate::scripts::DeviceLikeWrapper($devices_name[stringify!($devices)]);)*
                    $(let $args: $args_ty = crate::value::FromValue::from_value(args[stringify!($args)].clone())?;)*
                    $body
                }

                fn devices(&$self) -> Vec<String> {
                    vec![$(stringify!($devices).to_owned()),*]
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
mod test;

use beta::BetaScripts;
use micro_r2::MicroR2Scripts;
use test::TestScripts;

script_config!(MicroR2Scripts => "micro-r2", BetaScripts => "beta", TestScripts => "test_model");
