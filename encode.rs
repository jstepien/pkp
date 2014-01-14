#[crate_type = "bin"];

use std::io::{stdin, stdout};
use std::trie::TrieMap;

type Palette = TrieMap<u8>;

fn emit(bytes: &[u8]) {
  stdout().write(bytes)
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
  let mut bytes = ~[(pal.len() - 1) as u8];
  let mut pairs = ~[];
  for (col, idx) in pal.iter() {
    pairs.push((*idx, col));
  };
  pairs.sort();
  for pair in pairs.iter() {
    let (_, col) = *pair;
    [0, 8, 16, 24].map(|shift| {
      bytes.push((col >> *shift) as u8)
    });
  }
  emit(bytes)
}

fn emit_pixels(pal: &Palette, data: &[u8]) {
  let mut bytes = ~[];
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
  let push_fn = if colours <= 2 {
    |offset: uint| {
      bytes.push(at(0, offset) | at(1, offset) | at(2, offset) | at(3, offset) |
                 at(4, offset) | at(5, offset) | at(6, offset) | at(7, offset));
      8
    }
  } else if colours <= 4 {
    |offset: uint| {
      bytes.push(at(0, offset) | at(2, offset) | at(4, offset) | at(6, offset));
      4
    }
  } else if colours <= 16 {
    |offset: uint| {
      bytes.push(at(0, offset) | at(4, offset));
      2
    }
  } else {
    |offset: uint| {
      bytes.push(at(0, offset));
      1
    }
  };
  let total = data.len();
  let mut offset = 0;
  let max_buffer_size = 1024;
  while offset < total {
    offset += push_fn(offset) * 4;
    if bytes.len() >= max_buffer_size {
      emit(bytes);
      bytes.clear();
    }
  }
  emit(bytes)
}

fn main() {
  let args = std::os::args();
  assert!(args.len() == 2);
  let width: uint = from_str(args[1]).unwrap();
  let data = stdin().read_to_end();
  let palette = make_palette(data);
  assert!(palette.len() <= 0x100);
  emit([(width >> 8) as u8, (width & 0xff) as u8]);
  emit_palette(&palette);
  emit_pixels(&palette, data);
}
