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
% saveas records the figure at its logical size and enlarges it to the export
% resolution. A uicontrol grows with it whatever its Units: pixels, normalized
% and points describe the same 200 x 100 pixel button here.
%=============================================================================
close all
%=============================================================================
cases = {{'pixels', [50 50 200 100]}, ...
  {'normalized', [0.125 1 / 6 0.5 1 / 3]}, ...
  {'points', [37.5 37.5 150 75]}};
for k = 1:numel(cases)
  f = figure('Visible', 'off', 'Position', [100 100 400 300], 'Color', [0 0 0]);
  uicontrol('Parent', f, 'Style', 'pushbutton', 'Units', cases{k}{1}, ...
    'Position', cases{k}{2}, 'BackgroundColor', [1 0 0], 'String', '');
  name = [tempname(), '.png'];
  saveas(f, name);
  close(f);
  image = imread(name);
  rmfile(name);
  scale = size(image, 2) / 400;
  [rows, cols] = find((image(:, :, 1) > 200) & (image(:, :, 2) < 80) & (image(:, :, 3) < 80));
  assert_istrue(~isempty(rows));
  assert_istrue(abs((max(cols) - min(cols) + 1) - 200 * scale) <= 4);
  assert_istrue(abs((max(rows) - min(rows) + 1) - 100 * scale) <= 4);
  assert_istrue(abs(min(cols) - 50 * scale) <= 4);
  assert_istrue(abs(min(rows) - 150 * scale) <= 4);
end
%=============================================================================
