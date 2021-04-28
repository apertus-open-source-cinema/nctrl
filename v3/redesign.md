# NCTRL redesign
## General structure on the rust side
(This needs some work to be accurate wrt reading from a address and how the i2c regs for example on the ar0330 work).
```rust
trait RWAble {
    type Address;
    fn read(&mut [u8]) -> Result<(), I2CError>;
    fn write(&[u8]) -> Result<NumWritten, I2CError>;
}

trait I2CBus {
    fn read(I2CAddress address, &mut [u8]) -> Result<(), I2CError>;
    fn write(I2CAddress address, &[u8]) -> Result<NumWritten, I2CError>;

    fn device(I2CAddress address) -> I2CDevice<Self>;
}

#[derive(Constructible)]
struct I2CDevice<T: I2CBus> {
    bus: T,
    address: I2CAddress
}

impl<T: I2CBUs> RWAble for  I2CDevice<T> { ... }

#[derive(Constructible)]
struct I2CCDev {
    device_num: u8,
    device: I2CCDevInternal
}

impl I2CBus for I2CCdev { ... }

#[derive(Constructible)]
struct I2CCMux {
    mux_device: I2CMuxInternal
}


struct I2CMuxSubBus {
    mux_device: I2CMuxInternal,
    bus: u8
}

impl I2CBus for I2CCMuxSubBus { ... }

#[derive(Constructible)]
struct NamedRegisters<T: RWAble> {
    device: T,
    registers: RegisterDescription
}
```


<a id="org7319775"></a>

## config structure
Devicetree like structure, but the binding statements (the `@0x7`) get parsed by the type itself, allowing to express a variety of different things with them. (For the `I2CMux` this could be the bus, for a `I2CBus` this could be a specific device address, for `NamedRegisters` this could be the name of a register, etc).

```devicetree
i2c_mux0: i2c_mux {
    device = "/dev/i2cmux0";

    named_registers@0x7 {
        description = "ar0330_regs.yml";
    }
}

named_registers {
    device = &i2c_mux0@0x1;
    description = "pmic_regs.yml"
}
```

<a id="orge2a9690"></a>

## config parsing
Everything deriving `Constructible` gets collected by a `build.rs` script into a registry, that uses the `Constructor` to get a object from `Any` arguments.

This registry is runtime extensible, which can be used to have seperate crates that implement their own types or support dynamic types, from languages like python / lua.

```rust
unsafe trait Constructible {
    type Constructor : Constructor;
}

// To make this runtime generic we need to use Any and have no way to force the trait to return the correct type, thus these two traits are unsafe and should only be implemented by the build.rs script / the derive macro. 
unsafe trait Constructor {
    fn construct(HashMap<Map, dyn Any> args) -> Any;
}


fn parse(ConfigData data, Device parent) {
    for device in data.devices() {
        let args = HashMap::new();
        // Something like                vvvv
        //                named_registers@0x7
        if let Some(parent_ref) = data.parent_ref() {
            args.insert(device.parent_name) = parent.parse_parent_ref(parent_ref);
        }
        args.update(device.args());
        let device = DeviceTypeRegistry(device.ty).construct(args);
        parse(device.sub_data(), )
    }
}
```

<a id="org33a27d5"></a>

## RPC
Example: (exact details with lifetimes and so on still to be determined).
```rust
impl<T: RWAble> IntrospectibleRPC for struct NamedRegisters<T> {
    fn describe_rpcs(&self) -> Vec<RPCDescription> {
        let mut descriptions = vec![];
        for reg in self.registers {
            match reg.ty {
                RegisterType::String => {
                    descriptions.add(rpc_description!(format!("read_{}", reg.name), |&self| -> Result<String> {
                        let bytes = Vec::with_capacity(reg.bytes);
                        return reg.map(self.device.read(&reg.bytes)?)?;
                    }));
                    descriptions.add(rpc_description!(format!("write_{}", reg.name), |&self, value: String| -> Result<()> {
                        let bytes = reg.unmap(&value)?;
                        return self.device.write(bytes)?;
                    }));
                }
                _ => unimplemented!();
            }
        }
    }
}
```
Sends a description of the available rpc calls like this:
```rust
#[derive(Serialize, Deserialize)]
struct Test {
    a: u8,
    b: String,
}

#[derive(Serialize, Deserialize)]
struct Test2 {
    a: u8,
    b: Test,
}
```
Converted to 
```
{
    "Test": Struct(
        [
            Named {
                name: "a",
                value: U8,
            },
            Named {
                name: "b",
                value: Str,
            },
        ],
    ),
    "Test2": Struct(
        [
            Named {
                name: "a",
                value: U8,
            },
            Named {
                name: "b",
                value: TypeName(
                    "Test",
                ),
            },
        ],
    ),
}
```
RPC is done over tcp / unix domain socket, using `serde-generate` to (dynamically) generate serialization and deserialization code for c++, java, python , rust, go and c#, typescript. The on wire format will be a slim wrapper around serde bincode.
## TODO
- locking / exclusive access
    - This is much simplified by forgein connections no longer being stateless, but the exact semantics / implementation is still unclear.
- init order (this probably needs a queue where devices that are missing deps get skipped and pushed back into the queue, or we could build the dependency tree and do a topo sort)
- bindings to / embedding of dynamic languages. Everything here should enable to do this, but I need to think more about it, so see if something is missing
- dynamic config changes, should be quite easy, but have to pay attention to things like updating references to devices and locking to avoid dead locks.
- thread safety

