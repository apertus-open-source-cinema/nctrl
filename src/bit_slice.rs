use failure::{format_err, Error};

use crate::address::Slice;

// v is a vector of bytes in big endian order
// shift v by address.slice_start bits to the right
// and accumulate address.slice_end - address.slice_start bits
// padded to whole bytes with zeros
// modifies v inplace (&mut [u8] would also do, but then we couldn't truncate)
pub fn slice(mut v: Vec<u8>, slice: &Option<Slice>) -> Vec<u8> {
    match slice {
        None => v,
        Some(Slice { start: slice_start, end: slice_end }) => {
            if slice_start == slice_end {
                return vec![]
            };

            let len = v.len();
            let len = if len > 0 { len - 1 } else { len };
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
                let lower_half = v[len - i] >> bit_offset;
                let upper_half = shift_left_with_overflow(v[len - (i + 1)], (8 - bit_offset) as u8);

                v[len - write_idx] = lower_half | upper_half;
                write_idx += 1;
            }

            let i = byte_offset + complete_bytes;
            let lower_half = v[len - i] >> bit_offset;

            // we cross a byte boundary, so fetch the value from the next byte
            let upper_half =
                if bit_offset + end_bits > 8 { v[len - (i + 1)] << (8 - bit_offset) } else { 0 };

            v[len - write_idx] = (lower_half | upper_half) & (0xff >> (8 - end_bits));

            v.drain(0..(len - write_idx)); // truncate(write_idx + 1);

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
pub fn slice_write(dest: &mut [u8], value: Vec<u8>, slice: &Option<Slice>) -> Result<(), Error> {
    match slice {
        None => Err(format_err!(
            "tried to write sliced value from {:?} to {:?}, but slice was None",
            dest,
            value
        )),
        Some(Slice { start: slice_start, end: slice_end }) => {
            if slice_start == slice_end {
                return Ok(())
            };

            let value_len = value.len();
            let value_len = if value_len > 0 { value_len - 1 } else { 0 };
            let dest_len = dest.len();
            let dest_len = if dest_len > 0 { dest_len - 1 } else { 0 };

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
                let lower_half = value[value_len - read_idx] << bit_offset;
                let lower_mask = 0xff << bit_offset;

                dest[dest_len - i] = masked_write(lower_half, dest[dest_len - i], lower_mask);

                let upper_half =
                    shift_right_with_overflow(value[value_len - read_idx], 8 - bit_offset);
                let upper_mask = shift_right_with_overflow(0xff, 8 - bit_offset);

                dest[dest_len - (i + 1)] =
                    masked_write(upper_half, dest[dest_len - (i + 1)], upper_mask);

                read_idx += 1;
            }

            let i = byte_offset + complete_bytes;
            let lower_half = value[value_len - read_idx] << bit_offset;
            let lower_mask = (0xff >> (8 - end_bits)) << bit_offset;

            dest[dest_len - i] = masked_write(lower_half, dest[dest_len - i], lower_mask);

            // we cross a byte boundary, so write the upper part
            if bit_offset + end_bits > 8 {
                let upper_half =
                    shift_right_with_overflow(value[value_len - read_idx], 8 - bit_offset);
                let upper_mask = shift_right_with_overflow(0xff >> (8 - end_bits), 8 - bit_offset);

                dest[dest_len - (i + 1)] =
                    masked_write(upper_half, dest[dest_len - (i + 1)], upper_mask);
            };

            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        address::Slice,
        bit_slice::{shift_left_with_overflow, shift_right_with_overflow, slice, slice_write},
    };
    use rand::Rng;

    fn a(start: u8, end: u8) -> Option<Slice> {
        Some(Slice { start: start as u64, end: end as u64 })
    }

    #[test]
    fn slice_single_byte_normal() {
        for i in 0..8 {
            let v = vec![0xff, 0x0, 0b10101010];
            assert_eq!(vec![0b10101010 >> i], slice(v, &a(i, i + 8)))
        }
    }

    #[test]
    fn slice_single_byte_end_byte() {
        for i in 8..16 {
            let v = vec![0xff, 0x0, 0b10101010];
            assert_eq!(vec![shift_left_with_overflow(0xff, 16 - i)], slice(v, &a(i, i + 8)))
        }
    }

    #[test]
    fn slice_multi_byte_normal() {
        for i in 0..8 {
            let v = vec![0x0, 0xff, 0x0, 0b10101010];
            assert_eq!(
                vec![shift_left_with_overflow(0xff, 8 - i), 0b10101010 >> i],
                slice(v, &a(i, i + 16))
            )
        }
    }

    #[test]
    fn slice_multi_byte_end_byte() {
        for i in 0..8 {
            let v = vec![0b01010101, 0xff, 0x0, 0b10101010];
            assert_eq!(
                vec![
                    (0xff >> i) | shift_left_with_overflow(0b01010101, 8 - i),
                    shift_left_with_overflow(0xff, 8 - i),
                    0b10101010 >> i
                ],
                slice(v, &a(i, i + 24))
            )
        }
    }

    #[test]
    fn slice_write_single_bit_normal() {
        for i in 0..8 {
            let mut v = vec![0b10101010, 0x0];
            slice_write(&mut v, vec![0x1], &a(i, i + 1)).unwrap();
            assert_eq!(vec![0b10101010, 0x1 << i], v);
        }
    }

    #[test]
    fn slice_write_single_bit_end_byte() {
        for i in 8..16 {
            let mut v = vec![0x0, 0b01010101];
            slice_write(&mut v, vec![0x1], &a(i, i + 1)).unwrap();
            assert_eq!(vec![0x1 << (i - 8), 0b01010101], v);
        }
    }

    #[test]
    fn slice_write_multi_bit_normal() {
        for i in 0..8 {
            let mut v = vec![0b10101010, 0x0, 0x0];
            slice_write(&mut v, vec![0xff], &a(i, i + 8)).unwrap();
            assert_eq!(
                vec![0b10101010, shift_right_with_overflow(0xff, (8 - i) as usize), 0xff << i],
                v
            );
        }

        for i in 0..8 {
            let mut v = vec![0b10101010, 0x0, 0x0];
            slice_write(&mut v, vec![0b01010101], &a(i, i + 8)).unwrap();
            assert_eq!(
                vec![
                    0b10101010,
                    shift_right_with_overflow(0b01010101, (8 - i) as usize),
                    0b01010101 << i
                ],
                v
            );
        }
    }

    #[test]
    fn slice_write_multi_bit_end_byte() {
        for i in 8..16 {
            let mut v = vec![0b10101010, 0x0, 0x0];
            slice_write(&mut v, vec![0xff], &a(i, i + 8)).unwrap();
            assert_eq!(
                vec![
                    0b10101010 | shift_right_with_overflow(0xff, (16 - i) as usize),
                    0xff << (i - 8),
                    0x0
                ],
                v
            );
        }
    }

    #[test]
    fn slice_write_multi_byte_normal() {
        for i in 0..8 {
            let mut v = vec![0x0, 0x0, 0x0, 0x0];
            slice_write(&mut v, vec![0xf0, 0xff], &a(i, i + 16)).unwrap();
            assert_eq!(
                vec![
                    0x0,
                    shift_right_with_overflow(0xf0, (8 - i) as usize),
                    (0xf0 << i) | shift_right_with_overflow(0xff, (8 - i) as usize),
                    0xff << i
                ],
                v
            );
        }
    }

    #[test]
    fn slice_write_multi_byte_end_byte() {
        for i in 0..8 {
            let mut v = vec![0xff, 0x0, 0x0, 0x0];
            slice_write(&mut v, vec![0xf0, 0xff], &a(i + 8, i + 24)).unwrap();
            assert_eq!(
                vec![
                    (0xff << i) | shift_right_with_overflow(0xf0, (8 - i) as usize),
                    (0xf0 << i) | shift_right_with_overflow(0xff, (8 - i) as usize),
                    0xff << i,
                    0x0
                ],
                v
            );
        }
    }

    #[test]
    fn slice_write_slice_inverse_test() {
        let mut rng = rand::thread_rng();
        const MAX_LEN: u8 = 15;

        for _ in 0..10000 {
            let dest_len = rng.gen_range(1, MAX_LEN + 1);
            let value_len = rng.gen_range(1, dest_len + 1);
            let slice_start = rng.gen_range(0, dest_len * 8 + 1);
            let slice_end =
                rng.gen_range(slice_start, (dest_len * 8).min(value_len * 8 + slice_start) + 1);

            let mut dest = (0..dest_len).map(|_| rng.gen::<u8>()).collect::<Vec<u8>>();
            let value = (0..value_len).map(|_| rng.gen::<u8>()).collect::<Vec<u8>>();

            slice_write(&mut dest, value.clone(), &a(slice_start, slice_end)).unwrap();
            assert_eq!(
                slice(value, &a(0, slice_end - slice_start)),
                slice(dest, &a(slice_start, slice_end))
            )
        }
    }

    #[test]
    fn dummy_test() {
        let v = vec![0x00, 0x00, 0x00];

        for start in 1u8..17 {
            for i in start..25 {
                let slice = Some(Slice { start: (i - start) as u64, end: i as u64 });

                let v = &mut v.clone();

                slice_write(v, vec![0xff, 0xff], &slice).unwrap();

                assert!(v.iter().cloned().map(u8::count_ones).sum::<u32>() == (start as u32))
            }
        }
    }
}
