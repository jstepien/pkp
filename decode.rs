use std::io::{stdin, stdout};

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

fn to_rgba(colours: uint, palette: &[u8], pixel_data: &[u8]) -> Vec<u8> {
  let (pixels_count_mult, mask, step) = if colours <= 2 {
    (8, 1, 1)
  } else if colours <= 4 {
    (4, 3, 2)
  } else if colours <= 16 {
    (2, 0xf, 4)
  } else {
    (1, 0xff, 8)
  };
  let mut rgba = Vec::from_elem(pixel_data.len() * pixels_count_mult * 4, 0u8);
  write_pixels(rgba.as_mut_slice(), pixel_data, palette, mask, step);
  rgba
}

fn main() {
  let mut stdin = stdin();
  let output = {
    let header = stdin.read_exact(3).unwrap();
    let colours = (header[2] as uint) + 1;
    let palette = stdin.read_exact(4 * colours).unwrap();
    let pixel_data = stdin.read_to_end().unwrap();
    to_rgba(colours, palette.as_slice(), pixel_data.as_slice())
  };
  let _ = stdout().write(output.as_slice()).unwrap();
}
