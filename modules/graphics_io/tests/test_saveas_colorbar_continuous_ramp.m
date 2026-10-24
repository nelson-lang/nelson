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
out = [tempdir(), 'saveas_colorbar_continuous_ramp'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 420 320], 'Color', [1 1 1]);
imagesc([0 1; 0 1]);
colormap([0 0 1; 1 1 0]);
clim([0 1]);
cb = colorbar();
drawnow();
%=============================================================================
pngFile = [out, '/colorbar.png'];
svgFile = [out, '/colorbar.svg'];
saveas(f, pngFile);
saveas(f, svgFile);
pngImage = imread(pngFile);
assert_valid_svg(svgFile, [420 320], 'polygon|image');
%=============================================================================
height = size(pngImage, 1);
width = size(pngImage, 2);
pos = cb.Position;
left = max(1, floor(pos(1) * width) + 3);
right = min(width, ceil((pos(1) + pos(3)) * width) - 3);
top = max(1, floor((1 - pos(2) - pos(4)) * height) + 3);
bottom = min(height, ceil((1 - pos(2)) * height) - 3);
column = round((left + right) / 2);
samples = reshape(pngImage(top:bottom, column, :), [], 3);
uniqueColors = unique(samples, 'rows');
asserts.istrue(size(uniqueColors, 1) >= 40);
%=============================================================================
close(f);
%=============================================================================
