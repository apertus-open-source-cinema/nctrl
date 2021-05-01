use std::{
    any::Any, cell::RefCell, collections::HashMap, future::Future, marker::PhantomData, pin::Pin,
    rc::Rc,
};

use anyhow;
use async_std::task;
use futures::future::{BoxFuture, FutureExt};
use generational_arena::{Arena, Index};
// use diff_derive::Diff;

// macro_rules! hash_map {
//     ($($k:expr => $v:expr),*) => {
//         {
//             let mut map = std::collections::HashMap::new();
//             $(map.insert($k, $v);)*
//             map
//         }
//     }
// }

struct B;

struct A<'a> {
    parent: &'a B,
}

impl B {
    async fn load() -> Self {
        println!("constructing B");
        Self
    }

    // async fn unload() -> Self {
    //     println!("deconstructing B");
    //     Self
    // }
}

impl Drop for B {
    fn drop(&mut self) { println!("deconstructing B") }
}

type ConstructorKey = u8;

impl<'a> A<'a> {
    async fn load(parent: &'a B) -> A<'a> {
        println!("constructing A");
        Self { parent }
    }

    // async fn unload(&mut self) {
    //     println!("deconstructing A");
    // }
}

impl<'a> Drop for A<'a> {
    fn drop(&mut self) { println!("deconstructing A") }
}

struct DagConstructor {
    store: RefCell<Arena<Pin<Box<dyn Any>>>>,
    deps: RefCell<HashMap<Index, Vec<Index>>>,
    dependents: RefCell<HashMap<Index, Vec<Index>>>,
}

#[derive(Clone)]
struct TimedIndex<'a> {
    index: Index,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> TimedIndex<'a> {
    fn new(index: Index) -> Self { Self { index, _phantom: PhantomData } }
}

impl DagConstructor {
    fn new() -> Self {
        Self {
            store: RefCell::new(Arena::new()),
            deps: RefCell::new(HashMap::new()),
            dependents: RefCell::new(HashMap::new()),
        }
    }

    async fn construct_no_args<'a, F: Future<Output = Pin<Box<dyn Any>>>>(
        &'a self,
        constructor: impl FnOnce() -> F,
    ) -> TimedIndex<'a> {
        let constructed = constructor().await;
        let idx = self.store.borrow_mut().insert(constructed);

        self.deps.borrow_mut().insert(idx, vec![]);
        self.dependents.borrow_mut().insert(idx, vec![]);

        TimedIndex::new(idx)
    }

    async fn construct<
        'a,
        F: Future<Output = Pin<Box<dyn Any>>> + 'a,
        G: FnOnce(Vec<&'a Pin<Box<dyn Any>>>) -> F + 'a,
    >(
        &'a self,
        constructor: G,
        deps: Vec<impl Future<Output = TimedIndex<'a>> + 'a>,
    ) -> TimedIndex<'a> {
        let mut awaited_deps = vec![];
        let mut deps_indices = vec![];
        for d in deps {
            let idx = d.await.index;
            deps_indices.push(idx);
            awaited_deps
                .push(unsafe { std::mem::transmute(self.store.borrow().get(idx).unwrap()) });
        }

        let constructed = constructor(awaited_deps).await;
        let idx = self.store.borrow_mut().insert(constructed);

        self.deps.borrow_mut().insert(idx, deps_indices.clone());
        {
            let mut dependents = self.dependents.borrow_mut();
            for dep_idx in deps_indices {
                dependents.get_mut(&dep_idx).unwrap().push(idx);
            }
        }
        self.dependents.borrow_mut().insert(idx, vec![]);

        TimedIndex::new(idx)
    }

    // TODO(robin): properly remove entries from deps and dependents
    fn deconstruct<'a>(
        &'a self,
        idx: Index,
    ) -> Pin<Box<dyn Future<Output = Result<(), String>> + 'a>> {
        Box::pin(async move {
            let dependents = self.dependents.borrow().get(&idx).unwrap().clone();

            for dependent in dependents {
                self.deconstruct(dependent).await.unwrap();
            }

            self.store.borrow_mut().remove(idx).unwrap();

            Ok(())
        })
    }
}

// TODO(robin): override Drop for DagConstructor, currently the ordering is
// wrong
fn main() -> anyhow::Result<()> {
    let constructor = DagConstructor::new();
    let a = task::block_on(async {
        let b = constructor.construct_no_args(|| async { Box::pin(B::load().await) as _ }).shared();

        let a = constructor.construct(
            |deps| async move {
                let any_ptr: *const dyn Any = &*deps[0];
                Box::pin(A::load(unsafe { &*(any_ptr as *const B) }).await) as _
            },
            vec![b.clone()],
        );

        a.await;
        b.await
    });
    task::block_on(async { constructor.deconstruct(a.index).await.unwrap() });
    println!("done");

    Ok(())
}
// let a = vec![(1, 2), (3, 4), (5, 6), (7, 8)];
// let b = vec![(3, 4), (1, 2), (5, 6), (7, 8)];

// println!("{:#?}", a.diff(&b));

// let old = hash_map!("1" => 1, "2" => 2, "3" => 3);
// let new = hash_map!("1" => 2, "2" => 2, "3" => 3);

// let old = hash_map!("2" => 2, "3" => 3);
// let new = hash_map!("1" => 2, "2" => 2, "3" => 3);

// let old = hash_map!("1" => 2, "2" => 2, "3" => 3);
// let new = hash_map!("2" => 2, "3" => 3);

// let old = hash_map!("1" => (1, 2), "2" => (3, 4), "3" => (5, 6));
// let new = hash_map!("1" => (3, 3), "2" => (1, 2), "3" => (5, 6));
// println!("{:#?}", old.diff(&new));

// fn main() -> anyhow::Result<()> {
//     let source = SourceFile {
//         file_name: "<embedded>",
//         contents: r#"fpga_config: fpga_mgr {
//     bitstream = <i2c.bit>;
//     tags = [
//         "has_i2c_mux",
//         "has_mipi_ar0330"
//     ];
//     config = (
//         i2c_mux_address = 0x07800200;
//     );
// }

// i2c_mux_overlay: dtc_overlay {
//     address = &fpga_config@i2c_mux_address;
//     dtc_template = <i2c_mux.dtc>;
// }

// i2c_mux0: i2c_mux {
//     device = &i2c_mux_overlay@device;

//     ar0330_regs: named_registers@0x7 {
//         description = <ar0330_regs.yml>;
//     }
// }

// ar0330_init: on_load {
//     lua_script = "ar0330_regs.write_analog_gain(1.0);";
//     python_script = `ar0330_regs.write_analog_gain(1.0)
// print("hello")
// `;
// }

// named_registers {
//     device = &i2c_mux0@0x1;
//     description = <pmic_regs.yml>;
// }"#,
//     };
//     let ast = parse(source)?;

//     // TODO(robin): integrate parse into Ast::parse_source or something like
// that.     println!("{:#?}", Ast::from_raw(source, ast)?);

//     Ok(())
// }
