%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
addpath([modulepath('graphics_io'), '/tests/helpers']);
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
addpath([modulepath('graphics', 'tests')]);
out = [tempdir(), 'saveas_contourslice_colorbar_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100, 100, 560, 420], 'Color', [1 1 1]);
[X, Y, Z] = meshgrid(-2:.2:2);
V = X .* exp(-X .^ 2 - Y .^ 2 - Z .^ 2);
xslice = [-1.2, 0.8, 2];
levels = -0.2:0.01:0.4;
contourslice(X, Y, Z, V, xslice, [], [], levels);
cb = colorbar();
view(3);
grid on;
drawnow();
%=============================================================================
pngFile = [out, '/contourslice_2.png'];
svgFile = [out, '/contourslice_2.svg'];
saveas(f, pngFile);
saveas(f, svgFile);
assert_valid_svg(svgFile, [560 420], 'polygon|image');
pngImage = imread(pngFile);
asserts.istrue(colorbarUniqueColorCount(pngImage, cb.Position) >= 80);
close(f);
%=============================================================================
function count = colorbarUniqueColorCount(imageData, position)
  height = size(imageData, 1);
  width = size(imageData, 2);
  left = max(1, floor(position(1) * width) + 3);
  right = min(width, ceil((position(1) + position(3)) * width) - 3);
  top = max(1, floor((1 - position(2) - position(4)) * height) + 3);
  bottom = min(height, ceil((1 - position(2)) * height) - 3);
  column = round((left + right) / 2);
  samples = reshape(imageData(top:bottom, column, :), [], 3);
  count = size(unique(samples, 'rows'), 1);
end
%=============================================================================
