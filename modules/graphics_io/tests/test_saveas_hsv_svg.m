%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'saveas_hsv_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420], 'Color', [1 1 1]);
surf(peaks);
colormap('hsv');
drawnow();
pngFile = [out, '/hsv.png'];
svgFile = [out, '/hsv.svg'];
saveas(f, pngFile);
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [560 420], 'image');
svgText = svgInfo.text;
asserts.isequal(contains(svgText, '<image '), true);
% The opaque surface bakes to a single raster <image> (its data stays crisp)
% while the axes, gridlines and labels export as vector. Only the handful of 3D
% axis wall panes appear as vector polygons; the surface itself is never a
% polygon soup.
polygonCount = numel(regexp(svgText, '<polygon', 'start'));
asserts.istrue(polygonCount < 50);
pngImage = imread(pngFile);
asserts.istrue(uniqueColorCount(pngImage) >= 128);
close(f);
%=============================================================================
function count = uniqueColorCount(imageData)
  pixels = reshape(imageData, [], 3);
  count = size(unique(pixels, 'rows'), 1);
end
%=============================================================================
