use crate::address::{Address, Slice};

// v is a vector of bytes in big endian order
// shift v by address.slice_start bits to the right
// and accumulate address.slice_end - address.slice_start bits
// padded to whole bytes with zeros
// modifies v inplace (&mut [u8] would also do, but then we couldn't truncate)
pub fn slice(mut v: Vec<u8>, address: &Address) -> Vec<u8> {
    match address.slice {
        None => v,
        Some(Slice { start: slice_start, end: slice_end }) => {
            let byte_offset = (slice_start >> 3) as usize;
            let bit_offset = (slice_start % 8) as usize;
            let bits = (slice_end - slice_start) as usize;

            // round up to nearest multiple of 8
            let complete_bytes = (bits + 7) >> 3;

            // this is nearly equivalent to bits % 8,
            // just not when bits % 8 == 0 (then this is 8)
            let end_bits = 8 - ((complete_bytes << 3) - bits);

            // we need to handle the last nibble manually,
            // because we wont always have a byte coming after that
            let complete_bytes = complete_bytes - 1;

            let mut write_idx = 0;

            for i in byte_offset..(byte_offset + complete_bytes) {
                let lower_half = v[i] >> bit_offset;
                let upper_half = shift_left_with_overflow(v[i + 1], (8 - bit_offset) as u8);

                v[write_idx] = lower_half | upper_half;
                write_idx += 1;
            }

            let i = byte_offset + complete_bytes;
            let lower_half = v[i] >> bit_offset;

            // we cross a byte boundary, so fetch the value from the next byte
            let upper_half =
                if bit_offset + end_bits > 8 { v[i + 1] << (8 - bit_offset) } else { 0 };

            v[write_idx] = (lower_half | upper_half) & (0xff >> (8 - end_bits));

            v.truncate(write_idx + 1);

            v
        }
    }
}

// write bit from val to cur where m is set
fn masked_write(val: u8, cur: u8, m: u8) -> u8 { (val & m) | (cur & !m) }

// this ugly casting is because 0xffu8 << 8 is counted as a overflow :(
fn shift_left_with_overflow(val: u8, shift: u8) -> u8 { ((u32::from(val) << shift) & 0xff) as u8 }

fn shift_right_with_overflow(val: u8, shift: usize) -> u8 {
    ((u32::from(val) >> shift) & 0xff) as u8
}

// write the contents of value to dest
// starting from address.slice_start
// stopping at address.slice_end bits
pub fn slice_write(dest: &mut [u8], value: Vec<u8>, address: &Address) {
    assert!(
        address.slice.is_some(),
        "slice_write doesn't do anything if address doesn't contain a slice"
    );

    match address.slice {
        None => (),
        Some(Slice { start: slice_start, end: slice_end }) => {
            let byte_offset = (slice_start >> 3) as usize;
            let bit_offset = (slice_start % 8) as usize;
            let bits = (slice_end - slice_start) as usize;

            // round up to nearest multiple of 8
            let complete_bytes = (bits + 7) >> 3;

            // this is nearly equivalent to bits % 8,
            // just not when bits % 8 == 0 (then this is 8)
            let end_bits = 8 - ((complete_bytes << 3) - bits);

            // we need to handle the last nibble manually,
            // because we wont always have a byte coming after that
            let complete_bytes = complete_bytes - 1;

            let mut read_idx = 0;

            for i in byte_offset..(byte_offset + complete_bytes) {
                let lower_half = value[read_idx] << bit_offset;
                let lower_mask = 0xff << bit_offset;

                dest[i] = masked_write(lower_half, dest[i], lower_mask);

                let upper_half = shift_right_with_overflow(value[read_idx], 8 - bit_offset);
                let upper_mask = shift_right_with_overflow(0xff, 8 - bit_offset);

                dest[i + 1] = masked_write(upper_half, dest[i + 1], upper_mask);

                read_idx += 1;
            }

            let i = byte_offset + complete_bytes;
            let lower_half = value[read_idx] << bit_offset;
            let lower_mask = (0xff >> (8 - end_bits)) << bit_offset;

            dest[i] = masked_write(lower_half, dest[i], lower_mask);

            // we cross a byte boundary, so write the upper part
            if bit_offset + end_bits > 8 {
                let upper_half = shift_right_with_overflow(value[read_idx], 8 - bit_offset);
                let upper_mask = shift_right_with_overflow(0xff >> (8 - end_bits), 8 - bit_offset);

                dest[i + 1] = masked_write(upper_half, dest[i + 1], upper_mask);
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        address::{Address, Slice},
        bit_slice::slice_write,
    };

    #[test]
    fn dummy_test2() {
        let address = Address { base: vec![], slice: Some(Slice { start: 3, end: 4 }) };

        let mut dest = vec![0x03, 0x00];

        let v = vec![0x1];

        slice_write(&mut dest, v, &address);

        println!("dest {:08b}", dest[0]);
        // panic!();
    }

    #[test]
    fn dummy_test() {
        let v = vec![0x00, 0x00, 0x00];

        for start in 1u8..17 {
            for i in start..25 {
                let address =
                    Address { base: vec![], slice: Some(Slice { start: i - start, end: i }) };

                let v = &mut v.clone();

                slice_write(v, vec![0xff, 0xff], &address);

                assert!(v.iter().cloned().map(u8::count_ones).sum::<u32>() == (start as u32))
            }
        }
    }
}
