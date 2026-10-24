%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function newcolors = fliplightness(colors)
  % Darken light colors and lighten dark colors, keeping their hue.
  % Oklab lightness L is flipped so that L^(3/2) becomes 1 - L^(3/2),
  % hue and chroma are kept, and the chroma is reduced to the largest
  % value inside the sRGB gamut when the flipped color falls outside.
  narginchk(1, 1);
  nargoutchk(0, 1);
  if ischar(colors) || isstring(colors) || iscell(colors)
    newcolors = flipTextColors(colors);
  else
    newcolors = flipNumericColors(colors);
  end
end
%=============================================================================
function newcolors = flipTextColors(colors)
  names = cellstr(colors);
  names = names(:);
  rgb = flipRGB(validatecolor(names, 'multiple'));
  if isempty(names) || ~all(startsWith(names, '#'))
    newcolors = rgb;
    return
  end
  nbColors = size(rgb, 1);
  hexDigits = dec2hex(reshape(round(rgb * 255).', [], 1), 2);
  hexCodes = [repmat('#', nbColors, 1), reshape(hexDigits.', 6, nbColors).'];
  if ischar(colors)
    newcolors = hexCodes;
  elseif isstring(colors)
    newcolors = reshape(string(cellstr(hexCodes)), size(colors));
  else
    newcolors = reshape(cellstr(hexCodes), size(colors));
  end
end
%=============================================================================
function newcolors = flipNumericColors(colors)
  if ~isnumeric(colors) || ~isreal(colors)
    error('Nelson:graphics:fliplightness:InvalidColors', _('Specify colors as a three-column matrix of RGB triplets, a character vector, a cell-array of character vectors, or a string array.'));
  end
  sz = size(colors);
  if ~((numel(sz) == 2 && sz(2) == 3) || (numel(sz) == 3 && sz(3) == 3))
    error('Nelson:graphics:fliplightness:InvalidNumericShape', _('Specify a color array as an m-by-3 RGB matrix or an m-by-n-by-3 truecolor array.'));
  end
  classname = class(colors);
  rgb = reshape(double(colors), [], 3);
  if isinteger(colors)
    offset = double(intmin(classname));
    span = double(intmax(classname)) - offset;
    rgb = (rgb - offset) / span;
    newcolors = cast(round(flipRGB(rgb) * span + offset), classname);
  else
    if ~all(rgb(:) >= 0 & rgb(:) <= 1)
      error('Nelson:graphics:fliplightness:OutOfRange', _('Specify double or single values in the range [0, 1].'));
    end
    newcolors = cast(flipRGB(rgb), classname);
  end
  newcolors = reshape(newcolors, sz);
end
%=============================================================================
function rgb = flipRGB(rgb)
  if isempty(rgb)
    return
  end
  % Oklab of the input: cube roots of the LMS responses.
  lab = (max(srgbToLinear(rgb) * rgbToLmsMatrix().', 0) .^ (1 / 3)) * lmsRootsToLabMatrix().';
  L = min(max(lab(:, 1), 0), 1);
  L = (1 - L .* sqrt(L)) .^ (1 / 3);
  L = L .* L;
  % Cube roots of the LMS responses of the flipped color with its chroma
  % scaled by s: base + s * slope, s = 1 keeps the whole chroma.
  M = labToLmsRootsMatrix();
  base = L * M(:, 1).';
  slope = lab(:, 2:3) * M(:, 2:3).';
  linear = lmsRootsToLinear(base + slope);
  outside = ~all(linear >= 0 & linear <= 1, 2);
  if any(outside)
    linear(outside, :) = reduceChroma(base(outside, :), slope(outside, :));
  end
  rgb = linearToSrgb(min(max(linear, 0), 1));
end
%=============================================================================
function linear = reduceChroma(base, slope)
  % Largest chroma scale in [0, 1] keeping lightness and hue inside the gamut.
  low = zeros(size(base, 1), 1);
  high = ones(size(base, 1), 1);
  for k = 1:16
    s = (low + high) / 2;
    linear = lmsRootsToLinear(base + s .* slope);
    inside = all(linear >= 0 & linear <= 1, 2);
    low = low + inside .* (s - low);
    high = high + ~inside .* (s - high);
  end
  linear = lmsRootsToLinear(base + low .* slope);
end
%=============================================================================
function linear = lmsRootsToLinear(q)
  linear = (q .* q .* q) * lmsToRgbMatrix().';
end
%=============================================================================
function M = rgbToLmsMatrix()
  % Oklab XYZ-to-LMS matrix times the sRGB (IEC 61966-2-1, D65) RGB-to-XYZ matrix.
  M = [0.41217645917703666, 0.53627397426958912, 0.051440372295501247;
    0.21190919958804832, 0.68071787098231307, 0.10739984387775423;
    0.088344814072132072, 0.28185396309857741, 0.6302808688015098];
end
%=============================================================================
function M = lmsToRgbMatrix()
  % Inverse of rgbToLmsMatrix.
  M = [4.0771868237173168, -3.3076225216643667, 0.23085919548795483;
    -1.2685764914005095, 2.6096871144850087, -0.34115574866072923;
    -0.0041965422316571979, -0.70339967610102627, 1.7067960338654125];
end
%=============================================================================
function M = lmsRootsToLabMatrix()
  % Oklab: cube roots of LMS to Lab.
  M = [0.2104542553, 0.7936177850, -0.0040720468;
    1.9779984951, -2.4285922050, 0.4505937099;
    0.0259040371, 0.7827717662, -0.8086757660];
end
%=============================================================================
function M = labToLmsRootsMatrix()
  % Inverse of lmsRootsToLabMatrix.
  M = [0.99999999845051957, 0.39633779217376774, 0.2158037580607588;
    1.0000000088817607, -0.10556134232365633, -0.063854174771705893;
    1.0000000546724108, -0.089484182094965739, -1.2914855378640917];
end
%=============================================================================
function linear = srgbToLinear(rgb)
  linear = ((rgb + 0.055) / 1.055) .^ 2.4;
  low = rgb <= 0.04045;
  linear = linear + low .* (rgb / 12.92 - linear);
end
%=============================================================================
function rgb = linearToSrgb(linear)
  rgb = 1.055 * linear .^ (1 / 2.4) - 0.055;
  low = linear <= 0.0031308;
  rgb = rgb + low .* (12.92 * linear - rgb);
end
%=============================================================================
