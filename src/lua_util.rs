use core::fmt::{self, Display};
use rlua::Error as LuaError;
use std::{fmt::Debug, sync::Arc};
use crate::camera;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct FailureCompat<E>(E);

impl FailureCompat<failure::Error> {
    pub fn failure_to_lua(e: failure::Error) -> LuaError {
        LuaError::ExternalError(Arc::new(FailureCompat(e)))
    }
}

impl<E: Display> Display for FailureCompat<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Lua error: ")?;
        Display::fmt(&self.0, f)
    }
}

impl<E: Display + Debug> std::error::Error for FailureCompat<E> {
    fn description(&self) -> &'static str { "An error has occurred." }
}

// i can't figure out the lifetimes for a function that would do this, so do a
// macro
#[macro_export(local_inner_macros)]
macro_rules! make_table {
    (@gen_read $scope:ident, $table:ident, $read_name:ident, $read:tt) => {
        let read =
            $scope.create_function(move |_, (_table, $read_name): (Table, String)| {
                $read
            })?;

        $table.set("__index", read)?;
    };
    (@gen_write $scope:ident, $table:ident, $write_name:ident, $write_val:ident, $write:tt) => {
        let write =
            $scope.create_function(move |_, (_table, $write_name, $write_val): (Table, String, String)| {
                $write
            })?;

        $table.set("__newindex", write)?;
    };
    ($context:ident, $scope:ident, |$read_name:ident| $read:tt) => {
        {
            use rlua::Table;

            let meta_table = $context.create_table()?;

            make_table!(@gen_read $scope, meta_table, $read_name, $read);

            let table = $context.create_table().map(|v| {
                v.set_metatable(Some(meta_table));
                v
            });

            let table: fuseable::Result<Table> = table.map_err(|e| e.into());
            table
        }
    };
    ($context:ident, $scope:ident, |$read_name:ident| $read:tt, |$write_name:ident, $write_val:ident| $write:tt) => {
        {
            use rlua::Table;

            let meta_table = $context.create_table()?;

            make_table!(@gen_read $scope, meta_table, $read_name, $read);
            make_table!(@gen_write $scope, meta_table, $write_name, $write_val, $write);

            let table = $context.create_table().map(|v| {
                v.set_metatable(Some(meta_table));
                v
            });

            let table: fuseable::Result<Table> = table.map_err(|e| e.into());
            table
        }

    };
}

pub fn create_lua_vm() -> rlua::Lua {
    let lua_vm = rlua::Lua::new();

    // TODO(robin): this could be optimized by `load`ing them all once at startup
    // however that also comes with some problems like globals depending on other globals
    lua_vm.context(|ctx| {
        let globals = ctx
            .create_function(|_, name: String| {
                camera::globals::<String>(&name).map_err(|e| FailureCompat::failure_to_lua(e.into()))
            })
            .unwrap();

        let meta_table = ctx.create_table().unwrap();

        let global_index = ctx.load(r#" function (table, name) local func, err = load("return " .. globals(name)) return func() end"#).eval::<rlua::Function>().unwrap();

        meta_table.set("__index", global_index).unwrap();

        ctx.globals().set_metatable(Some(meta_table));

        ctx.globals().set("globals", globals).unwrap();


        let script_get = ctx
            .create_function(|ctx,  (_table, name): (rlua::Table, String)| {
                camera::with_camera(|cam| {
                    println!("trying to read script {}", name);

                    let script = cam.scripts[&name].lock().unwrap();
                    let read_func = script.read_key();
                    let write_func = script.write_key();

                    let script_table = ctx.create_table()?;

                    read_func.map(|key| {
                        script_table.set("read", ctx.registry_value::<rlua::Function>(&key).unwrap()).unwrap();
                    });

                    write_func.map(|key| {
                        script_table.set("write", ctx.registry_value::<rlua::Function>(&key).unwrap()).unwrap();
                    });

                    Ok(script_table)
                })
            }).unwrap();

        let scripts_metatable = ctx.create_table().unwrap();
        scripts_metatable.set("__index", script_get).unwrap();

        let scripts = ctx.create_table().unwrap();
        scripts.set_metatable(Some(scripts_metatable));

        ctx.globals().set("scripts", scripts).unwrap();
    });

    lua_vm
}

pub struct LuaDeviceWrapper<'a>(pub rlua::Table<'a>);

// TODO(robin): error handling
// TODO(robin): figure out a way to get a empty table (maybe pass one in?)
impl<'a> LuaDeviceWrapper<'a> {
    #[allow(dead_code)]
    pub fn read_raw(&self, name: &str) -> std::result::Result<String, rlua::Error> {
//            .ok_or_else(|| format_err!("rust script was called from lua and the device {:?} had no metatable", self.0)).map_err(FailureCompat::failure_to_lua)?;
        let metatable =  self.0.get::<_, rlua::Table>("raw").unwrap().get_metatable().unwrap();
        metatable.get::<_, rlua::Function>("__index")?.call::<_, String>((self.0.clone(), name))
    }

    #[allow(dead_code)]
    pub fn write_raw<T: crate::device::ToStringOrVecU8 + rlua::ToLua<'a>>(&self, name: &str, value: T) -> std::result::Result<(), rlua::Error> {
        let metatable =  self.0.get::<_, rlua::Table>("raw").unwrap().get_metatable().unwrap();
        metatable.get::<_, rlua::Function>("__newindex")?.call((self.0.clone(), name, value))
    }

    #[allow(dead_code)]
    pub fn read_cooked(&self, name: &str) -> std::result::Result<String, rlua::Error> {
        let metatable =  self.0.get::<_, rlua::Table>("cooked").unwrap().get_metatable().unwrap();
        metatable.get::<_, rlua::Function>("__index")?.call::<_, String>((self.0.clone(), name))
    }

    #[allow(dead_code)]
    pub fn write_cooked<T: crate::device::ToStringOrVecU8 + rlua::ToLua<'a>>(&self, name: &str, value: T) -> std::result::Result<(), rlua::Error> {
        let metatable =  self.0.get::<_, rlua::Table>("cooked").unwrap().get_metatable().unwrap();
        metatable.get::<_, rlua::Function>("__newindex")?.call((self.0.clone(), name, value))
    }

    #[allow(dead_code)]
    pub fn read_computed(&self, name: &str) -> std::result::Result<String, rlua::Error> {
        let metatable =  self.0.get::<_, rlua::Table>("computed").unwrap().get_metatable().unwrap();
        metatable.get::<_, rlua::Function>("__index")?.call::<_, String>((self.0.clone(), name))
    }

    #[allow(dead_code)]
    pub fn write_computed<T: crate::device::ToStringOrVecU8 + rlua::ToLua<'a>>(&self, name: &str, value: T) -> std::result::Result<(), rlua::Error> {
        let metatable =  self.0.get::<_, rlua::Table>("computed").unwrap().get_metatable().unwrap();
        metatable.get::<_, rlua::Function>("__newindex")?.call((self.0.clone(), name, value))
    }
}
