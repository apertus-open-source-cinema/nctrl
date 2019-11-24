#!/usr/bin/env python3

import ruamel.yaml as yaml
import xmltodict
from collections import OrderedDict
from math import ceil

def eprint(*args, **kwargs):
    import sys
    print(*args, file=sys.stderr, **kwargs)

with open('AR0330-REV2.xsdat') as fd:
    doc = xmltodict.parse(fd.read())

with open('AR0330-REV2.ldat') as fd:
    long_doc = xmltodict.parse(fd.read())

def maybe_do(reg, elem_name, f):
    if elem_name in reg:
        f()

def maybe_assign(reg, elem_name, target_reg, target_name):
    if elem_name in reg:
        target_reg[target_name] = reg[elem_name]

yml_doc = {}

stupid_names = [ "Reserved" ]

def add_reg(doc, reg, is_bitfield):
    yml_reg = {} 

    name = "unknown"

    def name_handler():
        nonlocal name
        name = reg["@name"]

    maybe_do(reg, "@name", name_handler)

    def display_name_handler():
        nonlocal name
        dname = reg["@display_name"]
        if dname not in stupid_names and " " not in dname:
            name = dname

    maybe_do(reg, "@display_name",
            display_name_handler)

    name = name.lower()

    if name is "unknown":
        raise("We fucked up the name")

    maybe_assign(reg, "@addr", yml_reg, "address")
    maybe_assign(reg, "@span", yml_reg, "width")
    if "width" in yml_reg:
        yml_reg["width"] = int(yml_reg["width"], 0)

    maybe_assign(reg, "@mask", yml_reg, "mask")
    maybe_assign(reg, "@range", yml_reg, "range")

    if "range" in yml_reg:
        r = yml_reg["range"]
        del yml_reg["range"]

        [min, max] = map(lambda s: int(s, 0), r.split(' '))
        yml_reg["min"] = min
        yml_reg["max"] = max

    if not is_bitfield:
        if "width" not in yml_reg and "max" in yml_reg:
            yml_reg["width"] = int(ceil((yml_reg["max"]).bit_length() / 8))
        if "width" not in yml_reg and "mask" in yml_reg:
            yml_reg["width"] = int(ceil(int(yml_reg["mask"], 0).bit_length() / 8))


    maybe_assign(reg, "@datatype", yml_reg, "datatype")
    maybe_assign(reg, "@rw", yml_reg, "rw")
    maybe_assign(reg, "@default", yml_reg, "default")
    maybe_assign(reg, "detail", yml_reg, "short_desc")

    # messy code, because we can't distinguish bitfield and normal reg   
    try: 
        regs = long_doc["sensor"]["registers"]["reg"]
        ldesc = None
        for l_reg in regs:
            if "bitfield" in l_reg:
                if isinstance(l_reg["bitfield"], OrderedDict):
                    l_reg["bitfield"] = [l_reg["bitfield"]]
    
                for bitfield in l_reg["bitfield"]:
                    if bitfield["@name"] == reg["@name"]:
                        ldesc = bitfield["long_desc"]
                        yml_reg["long_desc"] = ldesc
                        break

            if l_reg["@name"] == reg["@name"]:
                ldesc = l_reg["long_desc"]
                yml_reg["long_desc"] = ldesc
                break

        if ldesc is None:
            raise("bs")
    except:
        pass
        # print("no long doc for {} :(".format(reg["@name"]))

    def process_desc(desc): 
        import re
        rs = re.finditer("(R0x[0-9a-fA-F]{4})", desc)
        
        for r in rs:
            r = r.group(0)
            global doc
            for reg in doc["sensor"]["registers"]["reg"]:
                if "@addr" in reg and "@name" in reg: 
                    if reg["@addr"].lower() == r[1:].lower():
                        name = reg["@name"]
                        if "@display_name" in reg:
                            name = reg["@display_name"]

                        name = name.lower()

                        print(desc)
                        # desc = re.sub("R0x[0-9a-fA-F]{4}(-[0-9a-fA-F])?", name, desc)
                        desc = re.sub(r + "(-[0-9a-fA-F])?", name, desc, flags=re.IGNORECASE)
                        print(desc)
                        break

        return desc


    if "short_desc" in yml_reg and "long_desc" in yml_reg:
        short = yml_reg["short_desc"] 
        long = yml_reg["long_desc"] 
        short = process_desc(short)
        long = process_desc(long)

        del yml_reg["short_desc"]
        del yml_reg["long_desc"]
        
        if short == long:
            yml_reg["description"] = short
        else:
            desc = {}
            desc["short"] = short
            desc["long"] = long

            yml_reg["description"] = desc
    elif "short_desc" in yml_reg:
        short = yml_reg["short_desc"]
        short = process_desc(short)
        yml_reg["description"] = short
        del yml_reg["short_desc"]
    elif "long_desc" in yml_reg:
        long = yml_reg["long_desc"]
        long = process_desc(long)
        yml_reg["description"] = long
        del yml_reg["long_desc"]

    doc[name] = yml_reg

    return name

for reg in doc["sensor"]["registers"]["reg"]:
    n = add_reg(yml_doc, reg, False)

    bitfields = {}

    if "bitfield" in reg:
        if isinstance(reg["bitfield"], OrderedDict):
            reg["bitfield"] = [reg["bitfield"]]
    
        for bitfield in reg["bitfield"]:
            add_reg(bitfields, bitfield, True)

        yml_doc[n]["bitfields"] = bitfields    


with open("sensors/ar0330/raw.yml", "w") as outfile:
    yaml.dump(yml_doc, outfile, default_flow_style=False)


high_level_doc = {}

def ffs(x):
    # first set bit
    # yolo two's complement hacks
    return (x & -x).bit_length() - 1


def add_high_level_reg(doc, reg, name, extra_name=None):
    # convert mask to range
    mask = int(reg["mask"], 0)

    first_bit = ffs(mask)
    last_bit = mask.bit_length() - 1
    test_mask = (((2 ** (last_bit + 1)) - 1) >> first_bit) << first_bit

    if mask != test_mask:
        eprint("shitty mask:", hex(mask), "⇔", bin(mask))
   
    if extra_name is not None:
        addr_base = extra_name
    else:
        addr_base = name

    addr = "%s[%d:%d]" % (addr_base, first_bit, last_bit + 1)
    reg["address"] = addr
    del reg["mask"]

    if "width" in reg:
        del reg["width"]

    if "rw" in reg:
        if reg["rw"] == "RO":
            del reg["rw"]
            reg["writable"] = False
        else:
            eprint(name, ": unhandled rw type:", reg["rw"])
            exit(1)
    else:
        reg["writable"] = True

    
    if "range" in reg:
        r = reg["range"]
        del reg["range"]

        [min, max] = map(lambda s: int(s, 0), r.split(' '))
        reg["min"] = min
        reg["max"] = max



    if "short_desc" in reg and "long_desc" in reg:
        short = reg["short_desc"] 
        long = reg["long_desc"] 
        process_desc(long)
        process_desc(short)

        del reg["short_desc"]
        del reg["long_desc"]
        
        if short == long:
            reg["description"] = short
        else:
            desc = {}
            desc["short"] = short
            desc["long"] = long

            reg["description"] = desc
    elif "short_desc" in reg:
        short = reg["short_desc"]
        process_desc(short)
        reg["description"] = short
        del reg["short_desc"]
    elif "long_desc" in reg:
        long = reg["long_desc"]
        process_desc(long)
        reg["description"] = long
        del reg["long_desc"]

    if "default" in reg:
        reg["default"] = int(reg["default"], 0)
    
    if name in doc:
        print(name, "is doubled")
        i = 0
        new_name = "{}_collision_{}".format(name, i)
        while new_name in doc:
            i += 1
            new_name = "{}_collision_{}".format(name, i)

        name = new_name

    doc[name] = reg

for name, reg in yml_doc.items():
    if "bitfields" in reg: 
        # split into bitfields
        # TODO(robin): is this always the correct choice?

        for bit_name, bitfield in reg["bitfields"].items():
            add_high_level_reg(high_level_doc, bitfield, bit_name, name)
    else:
        # use whole register as target
        add_high_level_reg(high_level_doc, reg, name)

with open("sensors/ar0330/high.yml", "w") as outfile:
    yaml.dump(high_level_doc, outfile, default_flow_style=False)
