#[crate_type = "bin"];

use std::io::{stdin, stdout};
use std::vec;

fn emit(rgba: &mut [u8], pos: uint, palette: &[u8], pix: u8) -> uint {
  let base = 4 * (pix as uint);
  let mut offset = 0;
  while offset < 4 {
    rgba[pos + offset] = palette[base + offset];
    offset += 1;
  }
  return pos + 4;
}

fn write_pixels(rgba: &mut [u8], pixel_data: &[u8], palette: &[u8], mask: u8, step: uint) {
  let mut pos = 0;
  for b in pixel_data.iter() {
    let mut shift = 0;
    while shift < 8 {
      pos = emit(rgba, pos, palette, (*b >> shift) & mask);
      shift += step;
    }
  }
}

fn to_rgba(colours: uint, palette: &[u8], pixel_data: &[u8]) -> ~[u8] {
  let pixels_count_mult = if colours <= 2 {
    8
  } else if colours <= 4 {
    4
  } else if colours <= 16 {
    2
  } else {
    1
  };
  let mut rgba = vec::from_elem(pixel_data.len() * pixels_count_mult * 4, 0u8);
  if colours <= 2 {
    write_pixels(rgba, pixel_data, palette, 1, 1);
  } else if colours <= 4 {
    write_pixels(rgba, pixel_data, palette, 3, 2);
  } else if colours <= 16 {
    write_pixels(rgba, pixel_data, palette, 0xf, 4);
  } else {
    write_pixels(rgba, pixel_data, palette, 0xff, 8);
  };
  rgba
}

fn main() {
  let mut stdin = stdin();
  let header = stdin.read_bytes(3);
  let colours = (header[2] as uint) + 1;
  let palette = stdin.read_bytes(4 * colours);
  let pixel_data = stdin.read_to_end();
  stdout().write(to_rgba(colours, palette, pixel_data));
}
