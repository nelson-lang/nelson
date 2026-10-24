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
%=============================================================================
fig = uifigure('Visible', 'off', 'Name', 'uitable export', ...
  'Position', [100 100 760 380], 'Color', [1 1 1]);
expected = magic(4);
direct = uitable(fig, 'Data', expected, ...
  'ColumnName', {'A', 'B', 'C', 'D'}, 'Position', [20 40 330 300]);
group = uitabgroup(fig, 'Position', [390 40 350 300]);
tab = uitab(group, 'Title', 'Nested');
nested = uitable(tab, 'Data', expected, ...
  'ColumnName', {'A', 'B', 'C', 'D'}, 'Position', [10 10 320 250]);
drawnow();

assert_isequal(direct.DisplayData, expected);
assert_isequal(nested.DisplayData, expected);

frame = getframe(fig);
assert_uitable_regions_have_text(frame.cdata);

imageName = [tempname(), '.png'];
saveas(fig, imageName);
assert_istrue(isfile(imageName));
assert_uitable_regions_have_text(imread(imageName));
rmfile(imageName);

delete(fig);
%=============================================================================
function assert_uitable_regions_have_text(img)
  directArea = img(40:340, 20:350, :);
  nestedArea = img(80:330, 400:720, :);
  assert_istrue(count_dark_pixels(directArea) > 100);
  assert_istrue(count_dark_pixels(nestedArea) > 100);
end
%=============================================================================
function count = count_dark_pixels(img)
  dark = (img(:, :, 1) < 120) & (img(:, :, 2) < 120) & (img(:, :, 3) < 120);
  count = nnz(dark);
end
%=============================================================================
