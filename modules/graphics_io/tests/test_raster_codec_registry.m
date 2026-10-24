%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
% <--RELEASE ONLY-->
%=============================================================================
readwrite = {'png', 'jpg', 'jpeg', 'jfif', 'gif', 'webp', 'tiff', 'tif', ...
             'bmp', 'dib', 'tga', 'pbm', 'pgm', 'ppm', 'pnm', 'pcx'};
canonical = {'png', 'jpg', 'gif', 'webp', 'tiff', 'bmp', 'tga', ...
             'pbm', 'pgm', 'ppm', 'pnm', 'pcx', 'psd', 'hdr', 'pic'};
formats = imformats();
actual = cell(1, length(formats));
multipageCapabilities = zeros(1, length(formats)) == 1;
alphaCapabilities = zeros(1, length(formats)) == 1;
for k = 1:length(formats)
  actual{k} = formats(k).ext;
  multipageCapabilities(k) = formats(k).multipage;
  alphaCapabilities(k) = formats(k).alpha;
end
asserts.isequal(actual, canonical);
formatsAgain = imformats();
for k = 1:length(formatsAgain)
  asserts.isequal(formatsAgain(k).ext, canonical{k});
end
for k = 1:length(readwrite)
  info = imformats(readwrite{k});
  asserts.isfalse(isempty(info));
  asserts.isfalse(isempty(info.read));
  asserts.isfalse(isempty(info.write));
end
readonly = {'psd', 'hdr', 'rgbe', 'pic'};
for k = 1:length(readonly)
  info = imformats(readonly{k});
  asserts.isfalse(isempty(info));
  asserts.isfalse(isempty(info.read));
  asserts.istrue(isempty(info.write));
end
asserts.isequal(imformats('jpeg').ext, 'jpg');
asserts.isequal(imformats('jfif').ext, 'jpg');
asserts.isequal(imformats('tif').ext, 'tiff');
asserts.isequal(imformats('dib').ext, 'bmp');
asserts.isequal(imformats('rgbe').ext, 'hdr');
asserts.istrue(imformats('gif').multipage);
asserts.isfalse(imformats('tiff').multipage);
asserts.isequal(multipageCapabilities, [false false true false false false false false ...
                                       false false false false false false false]);
asserts.isequal(alphaCapabilities, [true false true true true true true false false ...
                                   false false false true false true]);
%=============================================================================
destination = [tempname(), '/'];
mkdir(destination);
rgb = uint8(reshape(mod(0:191, 256), 8, 8, 3));
alphaSource = uint8(reshape(mod((0:63) * 17, 256), 8, 8));
lossless = {'png', 'bmp', 'dib', 'tga', 'ppm', 'pnm', 'pcx'};
for k = 1:length(lossless)
  filename = [destination, 'roundtrip.', lossless{k}];
  imwrite(rgb, filename);
  decoded = imread(filename);
  asserts.isequal(size(decoded), [8, 8, 3]);
end
for format = {'pbm', 'pgm'}
  filename = [destination, 'gray.', format{1}];
  imwrite(rgb(:, :, 1), filename);
  decoded = imread(filename);
  asserts.isequal(size(decoded), [8, 8]);
end
% Every writable codec can be read without a useful filename extension.
signatureColor = {'png', 'jpg', 'gif', 'webp', 'tiff', 'bmp', 'tga', ...
                  'ppm', 'pnm', 'pcx'};
for k = 1:length(signatureColor)
  format = signatureColor{k};
  signatureFile = [destination, 'signature-', format, '.data'];
  imwrite(rgb, signatureFile, format);
  [decoded, decodedMap] = imread(signatureFile);
  if strcmp(format, 'gif')
    asserts.isequal(size(decoded), [8, 8]);
    asserts.isequal(size(decodedMap, 2), 3);
  else
    asserts.isequal(size(decoded), [8, 8, 3]);
    asserts.isequal(size(decodedMap), [0, 0]);
  end
end
signatureGray = {'pbm', 'pgm'};
for k = 1:length(signatureGray)
  format = signatureGray{k};
  signatureFile = [destination, 'signature-', format, '.data'];
  imwrite(rgb(:, :, 1), signatureFile, format);
  asserts.isequal(size(imread(signatureFile)), [8, 8]);
end
asciiPbm = [destination, 'ascii.pbm'];
asciiPgm = [destination, 'ascii.pgm'];
asciiPpm = [destination, 'ascii.ppm'];
writeBytes(asciiPbm, uint8(sprintf('P1\n2 1\n0 1\n')));
writeBytes(asciiPgm, uint8(sprintf('P2\n2 1\n15\n0 15\n')));
writeBytes(asciiPpm, uint8(sprintf('P3\n2 1\n255\n255 0 0 0 0 255\n')));
asserts.isequal(imread(asciiPbm), uint8([255 0]));
asserts.isequal(imread(asciiPgm), uint8([0 255]));
asserts.isequal(reshape(imread(asciiPpm), 2, 3), uint8([255 0 0; 0 0 255]));
% Aliases select their canonical codecs even without a matching extension.
aliasFormats = {'jpeg', 'jfif', 'tif', 'dib'};
for k = 1:length(aliasFormats)
  aliasFile = [destination, 'alias-', aliasFormats{k}, '.data'];
  imwrite(rgb, aliasFile, aliasFormats{k});
  asserts.isequal(size(imread(aliasFile)), [8, 8, 3]);
end
%=============================================================================
webpFile = [destination, 'alpha.webp'];
imwrite(rgb, webpFile, 'Alpha', alphaSource, 'Quality', 90);
[decoded, map, alpha] = imread(webpFile);
asserts.isequal(size(decoded), [8, 8, 3]);
asserts.isequal(size(map), [0, 0]);
asserts.isequal(alpha, alphaSource);
% Alpha-capable writable codecs preserve an opaque/transparent mask.
binaryAlpha = uint8(255 * ones(8, 8));
binaryAlpha(1:2:end, 1:2:end) = 0;
alphaFormats = {'png', 'gif', 'webp', 'tiff', 'bmp', 'tga'};
for k = 1:length(alphaFormats)
  format = alphaFormats{k};
  alphaFile = [destination, 'alpha-', format, '.', format];
  imwrite(rgb, alphaFile, 'Alpha', binaryAlpha, 'Quality', 100);
  [decoded, decodedMap, decodedAlpha] = imread(alphaFile);
  if strcmp(format, 'gif')
    asserts.isequal(size(decoded), [8, 8]);
    asserts.isequal(size(decodedMap, 2), 3);
  else
    asserts.isequal(size(decoded), [8, 8, 3]);
    asserts.isequal(size(decodedMap), [0, 0]);
  end
  asserts.isequal(decodedAlpha, binaryAlpha);
end
opaqueTiff = [destination, 'opaque-alpha.tiff'];
opaqueAlpha = uint8(255 * ones(8, 8));
imwrite(rgb, opaqueTiff, 'Alpha', opaqueAlpha);
[opaqueRgb, opaqueMap, opaqueAlphaOut] = imread(opaqueTiff);
asserts.isequal(size(opaqueRgb), [8, 8, 3]);
asserts.isequal(size(opaqueMap), [0, 0]);
asserts.isequal(opaqueAlphaOut, opaqueAlpha);
%=============================================================================
indices = uint8([0 1 2; 2 1 0]);
palette = [1 0 0; 0 1 0; 0 0 1];
indexedAlpha = uint8([255 128 0; 0 128 255]);
indexedFile = [destination, 'indexed.png'];
imwrite(indices, palette, indexedFile, 'Alpha', indexedAlpha);
[decodedIndices, decodedPalette, decodedAlpha] = imread(indexedFile);
asserts.isequal(decodedIndices, indices);
asserts.isequal(decodedPalette, palette);
asserts.isequal(decodedAlpha, indexedAlpha);
for format = {'pcx', 'gif'}
  indexedFile = [destination, 'indexed.', format{1}];
  imwrite(indices, palette, indexedFile);
  [decodedIndices, decodedPalette] = imread(indexedFile);
  asserts.isequal(size(decodedIndices), size(indices));
  asserts.isequal(size(decodedPalette, 2), 3);
end
%=============================================================================
gray16 = uint16(reshape(0:63, 8, 8) * 1000);
formats16 = {'png', 'pgm', 'tiff'};
for k = 1:length(formats16)
  filename = [destination, 'gray16.', formats16{k}];
  imwrite(gray16, filename);
  decoded = imread(filename);
  asserts.isequal(class(decoded), 'uint16');
  asserts.isequal(decoded, gray16);
end
target16 = [destination, 'preserved.bmp'];
writeBytes(target16, uint8('existing'));
asserts.checkerror('imwrite(gray16, target16)', ...
    _('uint16 images are only writable as PNG, TIFF or PGM.'));
asserts.isequal(char(readBytes(target16)), 'existing');
asserts.checkerror("imwrite(uint16(zeros(2, 2, 3)), [destination, 'bad16.png'])", ...
    _('uint16 images are only writable as PNG, TIFF or PGM.'));
asserts.checkerror("imwrite(gray16, palette, [destination, 'bad16-map.png'])", ...
    _('uint16 images are only writable as PNG, TIFF or PGM.'));
asserts.checkerror("imwrite(uint8(zeros(2, 2, 4)), [destination, 'bad-shape.png'])", ...
    _('Image data must be either MxN or MxNx3.'));
asserts.checkerror("imwrite(rgb, palette, [destination, 'bad-map.png'])", ...
    _('A colormap can only be used with an MxN indexed image.'));
asserts.checkerror("imwrite(rgb, [destination, 'bad-alpha.png'], 'Alpha', uint8(zeros(8, 8, 3)))", _('Wrong size for AlphaMap.'));
%=============================================================================
noise = uint8(reshape(mod((0:12287) * 73, 256), 64, 64, 3));
jpegLow = [destination, 'low.jpg'];
jpegHigh = [destination, 'high.jfif'];
imwrite(noise, jpegLow, 'Quality', 10, 'Comment', 'codec-comment', 'Author', 'codec-author');
imwrite(noise, jpegHigh, 'Quality', 95);
asserts.isequal(size(imread(jpegLow)), [64, 64, 3]);
asserts.isequal(size(imread(jpegHigh)), [64, 64, 3]);
highInfo = dir(jpegHigh);
lowInfo = dir(jpegLow);
asserts.istrue(highInfo.bytes > lowInfo.bytes);
jpegDefault = [destination, 'default.jpg'];
jpeg75 = [destination, 'quality75.jpg'];
imwrite(noise, jpegDefault);
imwrite(noise, jpeg75, 'Quality', 75);
asserts.isequal(readBytes(jpegDefault), readBytes(jpeg75));
webpLow = [destination, 'low.webp'];
webpHigh = [destination, 'high.webp'];
webpDefault = [destination, 'default.webp'];
webp75 = [destination, 'quality75.webp'];
imwrite(noise, webpLow, 'Quality', 10);
imwrite(noise, webpHigh, 'Quality', 95);
imwrite(noise, webpDefault);
imwrite(noise, webp75, 'Quality', 75);
webpHighInfo = dir(webpHigh);
webpLowInfo = dir(webpLow);
asserts.istrue(webpHighInfo.bytes > webpLowInfo.bytes);
asserts.isequal(readBytes(webpDefault), readBytes(webp75));
jpegBytes = readBytes(jpegLow);
asserts.istrue(hasPattern(jpegBytes, uint8('codec-comment')));
asserts.istrue(hasPattern(jpegBytes, uint8('Author: codec-author')));
multiLineMetadata = [destination, 'metadata-lines.jpg'];
imwrite(rgb, multiLineMetadata, 'Comment', ["line-one", "line-two"]);
asserts.istrue(hasPattern(readBytes(multiLineMetadata), uint8(sprintf('line-one\nline-two'))));
pngMetadata = [destination, 'metadata.png'];
imwrite(rgb, pngMetadata, 'Comment', 'png-comment', 'Author', 'png-author');
pngBytes = readBytes(pngMetadata);
asserts.istrue(hasPattern(pngBytes, uint8('png-comment')));
asserts.istrue(hasPattern(pngBytes, uint8('png-author')));
% Unsupported metadata is deliberately ignored.
imwrite(rgb, [destination, 'metadata.bmp'], 'Comment', 'ignored', 'Author', 'ignored');
tiffMetadata = [destination, 'metadata.tiff'];
imwrite(rgb, tiffMetadata, 'Comment', 'tiff-comment', 'Author', 'tiff-author');
tiffBytes = readBytes(tiffMetadata);
asserts.istrue(hasPattern(tiffBytes, uint8('tiff-comment')));
asserts.istrue(hasPattern(tiffBytes, uint8('tiff-author')));
gifMetadata = [destination, 'metadata.gif'];
imwrite(rgb, gifMetadata, 'Comment', 'gif-comment', 'Author', 'ignored');
asserts.istrue(hasPattern(readBytes(gifMetadata), uint8('gif-comment')));
%=============================================================================
firstFrame = uint8(zeros(4, 5, 3));
firstFrame(:, :, 1) = 255;
secondFrame = uint8(zeros(4, 5, 3));
secondFrame(:, :, 3) = 255;
gifAlpha = uint8(255 * ones(4, 5));
gifAlpha(1, 1) = 0;
animatedGif = [destination, 'animated.gif'];
imwrite(firstFrame, animatedGif, 'LoopCount', Inf, 'DelayTime', 0.25, 'Alpha', gifAlpha, ...
        'Comment', 'animation-comment');
imwrite(secondFrame, animatedGif, 'WriteMode', 'append', 'DelayTime', 0.50);
[gifIndices, gifMap, gifAlphaOut] = imread(animatedGif);
asserts.isequal(size(gifIndices), [4, 5]);
asserts.isequal(size(gifAlphaOut), [4, 5]);
asserts.isequal(gifAlphaOut(1, 1), uint8(0));
firstColor = round(gifMap(double(gifIndices(2, 2)) + 1, :) * 255);
asserts.isequal(firstColor, [255 0 0]);
gifBytes = readBytes(animatedGif);
asserts.isequal(countPattern(gifBytes, uint8([33 249 4])), 2);
asserts.istrue(hasPattern(gifBytes, uint8('NETSCAPE2.0')));
asserts.istrue(hasPattern(gifBytes, uint8('animation-comment')));
asserts.istrue(hasPattern(gifBytes, uint8([33 249 4 5 25 0])));
asserts.istrue(hasPattern(gifBytes, uint8([33 249 4 5 50 0])));
%=============================================================================
multipageTiff = [destination, 'multipage.tiff'];
writeBytes(multipageTiff, twoPageTiff());
firstPage = imread(multipageTiff);
asserts.isequal(size(firstPage), [1, 1, 3]);
asserts.isequal(reshape(firstPage, 1, 3), uint8([255 0 0]));
%=============================================================================
psdFile = [destination, 'sample.psd'];
psd = [uint8('8BPS'), 0 1, zeros(1, 6), 0 3, 0 0 0 1, 0 0 0 1, 0 8, 0 3, ...
       zeros(1, 12), 0 0, 255 0 128];
writeBytes(psdFile, uint8(psd));
asserts.isequal(size(imread(psdFile)), [1, 1, 3]);
hdrFile = [destination, 'sample.rgbe'];
hdr = [uint8(sprintf('#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n-Y 1 +X 1\n')), 128 64 32 129];
writeBytes(hdrFile, uint8(hdr));
asserts.isequal(size(imread(hdrFile)), [1, 1, 3]);
picFile = [destination, 'sample.pic'];
pic = [83 128 246 52, zeros(1, 84), uint8('PICT'), 0 1 0 1, ...
       zeros(1, 8), 0 8 0 224, 10 20 30];
writeBytes(picFile, uint8(pic));
asserts.isequal(size(imread(picFile)), [1, 1, 3]);
picAlphaFile = [destination, 'sample-alpha.pic'];
picAlpha = [83 128 246 52, zeros(1, 84), uint8('PICT'), 0 1 0 1, ...
            zeros(1, 8), 0 8 0 240, 10 20 30 64];
writeBytes(picAlphaFile, uint8(picAlpha));
[picRgb, picMap, picAlphaOut] = imread(picAlphaFile);
asserts.isequal(reshape(picRgb, 1, 3), uint8([10 20 30]));
asserts.isequal(size(picMap), [0, 0]);
asserts.isequal(picAlphaOut, uint8(64));
for format = {'psd', 'hdr', 'pic'}
  command = ['imwrite(rgb, [destination, ''readonly.', format{1}, '''])'];
  expected = ['Cannot save image file: ', destination, 'readonly.', format{1}, ...
              ' (unsupported image format: ', format{1}, ')'];
  asserts.checkerror(command, expected);
end
invalidFile = [destination, 'invalid.png'];
writeBytes(invalidFile, uint8([137 80 78 71 13 10 26 10]));
asserts.checkerror('imread(invalidFile)', sprintf(_('Impossible read image file: %s'), 'invalid PNG image'));
%=============================================================================
function writeBytes(filename, bytes)
  fid = fopen(filename, 'wb');
  asserts.istrue(fid > 0);
  fwrite(fid, uint8(bytes), 'uint8');
  fclose(fid);
end

function bytes = readBytes(filename)
  fid = fopen(filename, 'rb');
  asserts.istrue(fid > 0);
  bytes = transpose(fread(fid, Inf, 'uint8'));
  fclose(fid);
end

function result = hasPattern(bytes, pattern)
  result = countPattern(bytes, pattern) > 0;
end

function count = countPattern(bytes, pattern)
  count = 0;
  for i = 1:(length(bytes) - length(pattern) + 1)
    if isequal(bytes(i:i + length(pattern) - 1), pattern)
      count = count + 1;
    end
  end
end

function bytes = little16(value)
  bytes = uint8([mod(value, 256), floor(value / 256)]);
end

function bytes = little32(value)
  bytes = uint8([mod(value, 256), mod(floor(value / 256), 256), ...
                 mod(floor(value / 65536), 256), floor(value / 16777216)]);
end

function bytes = tiffEntry(tag, type, count, value)
  bytes = [little16(tag), little16(type), little32(count), little32(value)];
end

function bytes = tiffIfd(nextIfd, bitsOffset, pixelOffset)
  bytes = little16(10);
  bytes = [bytes, tiffEntry(256, 4, 1, 1)];
  bytes = [bytes, tiffEntry(257, 4, 1, 1)];
  bytes = [bytes, tiffEntry(258, 3, 3, bitsOffset)];
  bytes = [bytes, tiffEntry(259, 3, 1, 1)];
  bytes = [bytes, tiffEntry(262, 3, 1, 2)];
  bytes = [bytes, tiffEntry(273, 4, 1, pixelOffset)];
  bytes = [bytes, tiffEntry(277, 3, 1, 3)];
  bytes = [bytes, tiffEntry(278, 4, 1, 1)];
  bytes = [bytes, tiffEntry(279, 4, 1, 3)];
  bytes = [bytes, tiffEntry(284, 3, 1, 1)];
  bytes = [bytes, little32(nextIfd)];
end

function bytes = twoPageTiff()
  bytes = [uint8('II'), little16(42), little32(8)];
  bytes = [bytes, tiffIfd(144, 134, 140), little16(8), little16(8), little16(8)];
  bytes = [bytes, uint8([255 0 0 0])];
  bytes = [bytes, tiffIfd(0, 270, 276), little16(8), little16(8), little16(8)];
  bytes = [bytes, uint8([0 0 255])];
end
%=============================================================================
