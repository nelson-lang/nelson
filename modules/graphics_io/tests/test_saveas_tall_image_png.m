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
% Raster export of a tall image: the software rasterizer maps the first
% destination row / column of the image rect to a negative source offset and
% used to read rows before the pixel buffer (an access violation once the
% raster is a few thousand rows high, silent garbage below that).
im = uint8(zeros(4000, 1000, 3));
im(:, :, 1) = 200;
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
image(im);
pngFile = [tempname(), '.png'];
saveas(f, pngFile);
assert_istrue(isfile(pngFile));
rgb = imread(pngFile);
assert_istrue(size(rgb, 1) > 100 && size(rgb, 2) > 100);
% The image area is uniformly red: its centre pixel must be red, not garbage.
cy = round(size(rgb, 1) / 2);
cx = round(size(rgb, 2) / 2);
assert_isequal(rgb(cy, cx, :), reshape(uint8([200 0 0]), [1 1 3]));
rmfile(pngFile);
% A square raster above the former crash threshold also exports.
im2 = uint8(zeros(2000, 2000, 3));
im2(:, :, 3) = 150;
clf(f);
image(im2);
jpgFile = [tempname(), '.jpg'];
saveas(f, jpgFile);
assert_istrue(isfile(jpgFile));
rmfile(jpgFile);
close(f);
