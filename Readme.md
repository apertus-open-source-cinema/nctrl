<!--
SPDX-FileCopyrightText: © 2019 Robin Ole Heinemann <robin.ole.heinemann@gmail.com>
SPDX-License-Identifier: CC-BY-SA-4.0
-->

# AXIOM ctrl
A driver for controlling AXIOM cameras.

## Getting started
Currently a somewhat recent stable [rust compiler](https://www.rust-lang.org/tools/install)  is required. 
Furthermore you need to install `libfuse` and the development headers for it (called `libfuse-dev` on debian derivatives). 

Clone the repository using
```bash
git clone --recursive https://github.com/apertus-open-source-cinema/nctrl
```

In this directory you can use `cargo run` to start the control daemon. 
For example:
```bash
cargo run -- -d nctrl_mountpoint -m camera_descriptions/beta/beta.yml
```
This starts the control daemon with the `beta` registers and using `nctrl_mountpoint` as mountpoint for the fuse API.

## Working Principle
The code in this repository takes care of all the low level communication to the hardware
of the camera (ie. the image sensor). This is done with a variety of protocols (ie. `i2c`
or memory access to shared memory regions with the FPGA).

Similar to a Linux kernel driver, a filesystem hierarchy is exposed, which represents the
different parameters of the hardware.

Exposing the parameters as a filesystem allows for simple solutions for a wide veriety
of use cases:
1) Write/ Read single parameters:
    ```bash
    $ cat /axiom_api/devices/cmv12000/cooked/pga_gain/value
    1
    $ echo -n "2" > /axiom_api/devices/cmv12000/cooked/pga_gain/value # sets the analog gain to 2×
    ```
2) List available parameters:
    ```bash
    $ ls /axiom_api/devices/cmv12000/cooked/
	pga_gain pga_div ...
    ```
3) Get information about parameters:
    ```bash
    $ cat /axiom_api/devices/cmv12000/cooked/pga_gain/description
	analog gain
    ```

This simple abstraction allows to easily create powerful tools that build upon ctrl, like the register explorer of the [webui](https://github.com/axiom-micro/webui).

![webui screenshot](img/webui_screenshot.png)

## No Kernel Space Code
No kernel code is needed to expose the outlined functionality and `FUSE` is used
to implement the filesystem. This gives better debuggability and allows us to code
rust instead of kernel style C at the cost of some performance penalty and loosing the ability to handle
interrupts.


## Developing locally
> **:warning: This Project uses submodules!**  
> **Either use `git clone --recursive` or run `git submodule update --init -- recursive` after cloning, otherwise the build will fail!**
```bash
$ mkdir ./axiom_api
$ cargo run -- --mock --mountpoint ./axiom_api camera_descriptions/beta/beta.yml
```


## Concepts
The control daemon parses a `YAML` file that describes the camera setup, the available `devices`, globals / functions, lua `scripts` and initialization tasks.
### devices
The `devices` block lists the available devices and their parameters. Each `device` consist of four parts:
1) A communication channel, that specifies how registers are read and written. This can for example be a memory mapped region or a i2c device. The different communication channels are implemented in rust and the configuration file specifies the necessary parameters. For example
   ```yaml
   channel:
   mode: "i2c-cdev"
	 bus: 0
	 address: 0x10
   ``` 
2) Raw registers, that assign a address a name and potentially some metadata like the width of the register, a description, min and max values or a default. 
   ```yaml
   temp_sensor:
     address: 127
     width: 2
     default: 0
     description: >
        Read-Only. Contains a value for calculating the sensor temperature.
   ```
3) Cooked registers, that assign a bit slice of a raw register or a address a name, metadata like the raw registers and potentially a value map. This map can map the raw register values to either floats, ints or strings. If such a map is present, reading a cooked register automatically returns the value assigned by the map and writing to such a register converts the given value to the raw value using this map. For example:
   ```yaml
   pga_gain:
     address: pga[0:3]
     description: analog gain
     map:
       0: 1
       1: 2
       3: 3
       7: 4
   ```
   This assigns a raw value of 0 the *cooked* value 1, the raw value 1 the *cooked* value 2 and so forth. Writing 4 to this register would write 7 to the first three bits of the raw register `pga`. If the first three bits of the raw register `pga` contain the value 1 reading this register would return 2.
4) Computed registers, that allow for arbitrary lua scripts to read and write a combination of registers. This could for example be used to provide a way to directly set a ISO value, which then sets a combination of digital gain, analog gain and potentially other registers. For example: 
   ```yaml
   analog_gain:
     description: "Sets the analog gain"
     type: float
     get: return cooked.coarse_gain * cooked.fine_gain
     set: >
       local coarse = math.floor(value)
       local fine = value / coarse
       cooked.coarse_gain = coarse
       cooked.fine_gain = fine
   ```
   This would provide a computed register for setting and reading the analog gain on the `ar0330` image sensor.

### globals
The `globals` block provides a way to set globals like for example the name of the default bitstream to load, or the frequency of the clock that is provided by this bitstream to the image sensor.
```yaml
extclock: 24000000
default_bitstream: no_patch.bit.bin
```
When using globals from `lua` it is additionally possible to write lua to represent a global value. This can for example be used to provide global helper functions for lua scripts or build one global parameter from other globals. For example:
```yaml
gain: |
  function (reg_a, reg_b)
      return reg_a * reg_b
  end
default_gain: gain(1, 2)
```
In lua scripts the global variable `gain` would then be a function that takes two arguments and returns their product and the global variable `default_gain` would have the value of `2`.


**NOTE**: rust scripts see these global constants written in lua as the string containing the `lua` code and not their actual evaluated value!
### scripts
The `scripts` block allows to specify lua scripts. Lua scripts are snippets of `lua` code that interact with multiple `devices` at once. During the execution of a script, no other access to the devices used by the script is allowed. For example when starting up a image sensor a series of different registers writes of different devices is often necessary, which should not be interrupted by register accesses / writes by others.  

Each `script` has a `description`, a list of devices it `uses` and optionally a list of argument names and types `args`. The `lua` code can access the `raw`, `cooked` and `computed` registers of the devices by reading from / writing to the `device_name.{raw, cooked, computed}` table. For example to read the `analog_gain` `raw` register of the `ar0330` device it can use `ar0330.raw.analog_gain`. To assign a value to this register: `ar0330.raw.analog_gain`. Scripts can access `globals` simply by their name. A complete script looks like this: 
``` yaml
test:
  description: A simple test script
  uses:
    - ar0330
  script: |
    print("a", a)
    print("b", b)
    print("c", c)
    print("d", d)
    print(extclock)
    scripts.test2(devices, { arg1 = 1.23, arg2 = "test"})
    ar0330.raw.analog_gain = 3
    return ar0330.computed.analog_gain
  args:
    a: int
    b: float
    c: string
    d: binary
```
Scripts can call other scripts by using the `scripts` table. A script has two arguments, a table containing devices, this table is automatically provided in the `devices` variable and a optional table with arguments. Finally scripts can also return a value.


Scripts can be run from `FUSE` by reading the `value` file in their directory. The value returned by the script is then received. Arguments of scripts can be assigned by writing to files in the `args` subfolder.
### init
Finally the `init` block contains a lua script that is executed after the control daemon is started and before any other access is allowed. This allows to initialize important devices, like loading a bitstream to a FPGA or initializing power supplies. Example:
```yaml
init: |
  ar0330.computed.analog_gain = 2
```


## Rust scripts 
Similar to lua scripts it is also possible to write scripts in rust. These have the same interface as lua scripts and can even call each other. Rust scripts can for example be used to implement performance critical tasks. They are defined using the `script!` macro. For example:
```rust
script! {
    "hard resets the sensor and brings it into standby\n"
    Reset { test: u8 } => {
        (self, devices = { ar0330, sensor_io }) {
			println!("test argument {}", test);

            sensor_io.write_raw("reset", 1)?;

            std::thread::sleep(std::time::Duration::from_millis(10));

            sensor_io.write_raw("reset", 0)?;
            ar0330.write_cooked("software_reset", 0)?;
            ar0330.write_cooked("stream", 1)?;
			
			
            let _ret = run_script!("test", devices, {
                a: 123,
                b: 1.23,
                c: "test",
                d: vec![0u8, 34u8]
            })?;

            ().to_bytes()
        }
    }
}
```
This defines a script with a single argument: `test` (with type `u8`). It uses the devices `ar0330` and `sensor_io` and also calls the lua script `test` from before.
A longer script, that starts the `ar0330` image sensor in default settings on the `micro-r2` can be found [here](https://github.com/apertus-open-source-cinema/nctrl/blob/490a7469b4768ad82c6ebc37f24c080a94545492/src/scripts/micro_r2.rs#L26-L167).
