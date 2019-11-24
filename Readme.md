# AXIOM ctrl
A driver for controlling AXIOM cameras.

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
However, no kernel code is needed to expose the outlined functionality and `FUSE` is used 
to implement the filesystem. This gives better debuggability and allows us to code
rust instead of kernel style C at the cost of some performance penalty and loosing the ability to handle
interrupts.


## Developing locally
```bash
$ mkdir ./axiom_api
$ cargo run -- --mock --mountpoint ./axiom_api camera_descriptions/beta/beta.yml
``
