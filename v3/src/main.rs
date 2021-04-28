use anyhow;
use v3::config_parser::{parse, Ast};

fn main() -> anyhow::Result<()> {
    let source = r#"fpga_config: fpga_mgr {
    bitstream = <i2c.bit>;
    tags = [
        "has_i2c_mux",
        "has_mipi_ar0330"
    ];
    config = (
        i2c_mux_address = 0x07800200;
    );
}

i2c_mux_overlay: dtc_overlay {
    address = &fpga_config@i2c_mux_address;
    dtc_template = <i2c_mux.dtc>;
}

i2c_mux0: i2c_mux {
    device = &i2c_mux_overlay@device;

    ar0330_regs: named_registers@0x7 {
        description = <ar0330_regs.yml>;
    }
}

ar0330_init: on_load {
    lua_script = "ar0330_regs.write_analog_gain(1.0);";
    python_script = `ar0330_regs.write_analog_gain(1.0)
print("hello")
`;
}

named_registers {
    device = &i2c_mux0@0x1;
    description = <pmic_regs.yml>;
}"#;
    let ast = parse(source)?;

    println!("{:#?}", Ast::from_raw(&source, ast));

    Ok(())
}
