# PKP

**PKP** is a lossless image compression library with full transparency support
which (ab)uses [xz][xz] to do all the hard work. At the moment of writing PKP
stands for _Perversely Kompressed Pictures_, but I'm open for better expansions
of this acronym.

## Benchmarks

              Size         JPEG     PNG     PKP   PKP/PNG   Enc.  Dec.
    pic       1728x2376    669k     53k     41k       77%   1.40  0.76
    lena      512x512      197k    187k    180k       96%   0.23  0.08
    hn1212    1077x672     327k     61k     49k       79%   0.40  0.16

JPEGs were compressed using [ImageMagick][im]'s `convert` with quality 95%.
PNGs were compressed using [optipng][optipng] with the `-o7` flag. `Enc.` is
time of encoding RGBO data as PKP. `Dec.` is time of decoding PKP back to RGBO.

  - `pic` is [the black and white fax picture from the Calgary Corpus][pic],
  - `lena` is the legendary [Lena][lena] reduced to 256 colours,
  - `hn1212` is a screenshot of the [Hacker News main page][hn] taken in
    December 2012 and reduced to 128 colours.

## Usage

PKP offers a minimal C interface summarised in `pkp.h`. `pkp_compress`
compresses an image given in a raw RGBO format, i.e. 4 bytes per pixel: red,
green, blue and opacity. `pkp_decompress` does the opposite. A small programme
which shows how the library can be used can be found in `pkp.c`.

## Building

Be sure to have [xz][xz], [GHC][ghc] and Python installed. Afterwards execute

    ./waf configure --prefix /usr/local
    ./waf
    sudo ./waf install

## Behind the scenes

PKP uses an indexed image format and as a result it encodes images with at most
256 colours. All colours present in the picture are mapped to natural numbers.
The mapping is emitted to the output stream. Subsequently, for each pixel in
the image the number of its colour is emitted. Finally, all the output is
compressed using xz. Yes, it's that naïve.

The encoding and decoding part is implemented in Haskell. The interface defined
in `pkp.h` is implemented in C. The C part manages pipes between the
encoder/decoder and xz. Thanks to decoupling it's easy to work on raw,
uncompressed data.

## Rationale

[LZMA][lzma] is a lossless dictionary-based compression algorithm used in xz.
Compression ratios it achieves are typically better than in case of gzip or
bzip2. It has been gaining popularity for some time now. xz is used by several
open source projects for compressing their releases. Arch Linux and Gentoo are
two examples of popular distributions which serve xz-compressed packages.

Since xz is so good when it comes to compressing source code and text, why
wouldn't it behave equally good on something completely different, namely pixel
data? The short answer is _"because images have different statistical
characteristics than text"_. Well, let's give it a try anyway and see what an
experiment will uncover.

## Future directions

  - Colours' codes could have been encoded in a more involved way, for instance
    using [Huffman coding][huff].
  - Processing pixels of the image using horizontal lines can be replaced with a
    different scheme which preserves the locality. Cf. [Hilbert curve][hilb].
  - PKP is slow. Make it fast.

## Colophon

The name of this project serves as a homage to [Polskie Koleje Państwowe][pkp],
the main operator of Polish railways. In fact, this library and travelling with
PKP have much in common. Speed would be a good example of a common feature.
Compression of contents too.

## License

Copyright (C) 2012 Jan Stępień

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  [xz]: http://tukaani.org/xz/
  [ghc]: http://www.haskell.org/ghc/
  [pic]: http://corpus.canterbury.ac.nz/descriptions/calgary/pic.html
  [lena]: http://en.wikipedia.org/wiki/File:Lenna.png
  [im]: http://www.imagemagick.org/
  [optipng]: http://optipng.sourceforge.net/
  [hn]: http://news.ycombinator.com/
  [huff]: http://en.wikipedia.org/wiki/Huffman_coding
  [hilb]: http://en.wikipedia.org/wiki/Hilbert_curve
  [lzma]: http://en.wikipedia.org/wiki/LZMA
  [pkp]: http://en.wikipedia.org/wiki/Polish_State_Railways
