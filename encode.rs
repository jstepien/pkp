use std::io::{stdin, stdout};
use std::collections::TrieMap;

type Palette = TrieMap<u8>;

fn emit(bytes: &[u8]) {
  let _ = stdout().write(bytes).unwrap();
}

fn make_key(data: &[u8], offset: uint) -> uint {
  (
    (data[offset + 0] as uint << 0) +
    (data[offset + 1] as uint << 8) +
    (data[offset + 2] as uint << 16) +
    (data[offset + 3] as uint << 24)
  )
}

fn make_palette(data: &[u8]) -> Palette {
  let mut pos = 0;
  let mut last = 0;
  let mut pal = TrieMap::new();
  let byte_count = data.len();
  while pos < byte_count {
    let key = make_key(data, pos);
    if !pal.contains_key(&key) {
      pal.insert(key, last);
      last += 1;
    }
    pos += 4
  }
  pal
}

fn emit_palette(pal: &Palette) {
  let mut bytes = Vec::from_elem(1, pal.len() as u8 - 1);
  let mut pairs = Vec::new();
  for (col, idx) in pal.iter() {
    pairs.push((*idx, col));
  };
  pairs.sort();
  for pair in pairs.iter() {
    let (_, col) = *pair;
    for shift in [0, 8, 16, 24].iter() {
      bytes.push((col >> *shift) as u8)
    };
  }
  emit(bytes.as_slice())
}

fn emit_pixels(pal: &Palette, data: &[u8]) {
  let mut bytes = Vec::new();
  let colours = pal.len();
  let (shift_mult, mask) = if colours <= 2 {
    (4, 1)
  } else if colours <= 4 {
    (2, 3)
  } else if colours <= 16 {
    (1, 0xf)
  } else {
    (0, 0xff)
  };
  let at = |shift: uint, off: uint| {
    let code = *pal.find(&make_key(data, shift_mult * shift + off)).unwrap();
    (code & mask) << shift
  };
  let next_byte_fn = if colours <= 2 {
    |offset: uint| {
      (
        at(0, offset) | at(1, offset) | at(2, offset) | at(3, offset) |
        at(4, offset) | at(5, offset) | at(6, offset) | at(7, offset),
        8
      )
    }
  } else if colours <= 4 {
    |offset: uint| {
      (at(0, offset) | at(2, offset) | at(4, offset) | at(6, offset), 4)
    }
  } else if colours <= 16 {
    |offset: uint| {
      (at(0, offset) | at(4, offset), 2)
    }
  } else {
    |offset: uint| {
      (at(0, offset), 1)
    }
  };
  let total = data.len();
  let mut offset = 0;
  let max_buffer_size = 1024;
  while offset < total {
    let (byte, offset_diff) = next_byte_fn(offset);
    bytes.push(byte);
    offset += offset_diff * 4;
    if bytes.len() >= max_buffer_size {
      emit(bytes.as_slice());
      bytes.clear();
    }
  }
  emit(bytes.as_slice())
}

fn main() {
  let args = std::os::args();
  assert!(args.len() == 2);
  let width: uint = from_str(args[1].as_slice()).unwrap();
  let data = stdin().read_to_end().unwrap();
  let palette = make_palette(data.as_slice());
  assert!(palette.len() <= 0x100);
  emit([(width >> 8) as u8, (width & 0xff) as u8]);
  emit_palette(&palette);
  emit_pixels(&palette, data.as_slice());
}
