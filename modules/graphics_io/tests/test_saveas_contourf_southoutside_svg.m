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
out = [tempdir(), 'saveas_contourf_southoutside_svg'];
if ~isfolder(out)
  mkdir(out);
end
%=============================================================================
f = figure('Visible', 'off', 'Position', [100, 100, 560, 420], 'Color', [1 1 1]);
contourf(peaks);
colormap('parula');
cb = colorbar('southoutside');
drawnow();
%=============================================================================
pngFile = [out, '/contourf_southoutside.png'];
svgFile = [out, '/contourf_southoutside.svg'];
saveas(f, pngFile);
saveas(f, svgFile);
svgInfo = assert_valid_svg(svgFile, [560 420], 'image');
svgText = svgInfo.text;
asserts.isequal(contains(svgText, '<image '), true);
pngImage = imread(pngFile);
asserts.istrue(horizontalColorbarUniqueColorCount(pngImage, cb.Position) >= 80);
close(f);
%=============================================================================
function count = horizontalColorbarUniqueColorCount(imageData, position)
  height = size(imageData, 1);
  width = size(imageData, 2);
  left = max(1, floor(position(1) * width) + 3);
  right = min(width, ceil((position(1) + position(3)) * width) - 3);
  top = max(1, floor((1 - position(2) - position(4)) * height) + 3);
  bottom = min(height, ceil((1 - position(2)) * height) - 3);
  row = round((top + bottom) / 2);
  samples = reshape(imageData(row, left:right, :), [], 3);
  count = size(unique(samples, 'rows'), 1);
end
%=============================================================================
