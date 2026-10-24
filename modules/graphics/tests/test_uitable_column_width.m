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
fig = uifigure('Visible', 'off', 'Position', [100 100 360 220], 'Color', [1 1 1]);
widths = {28, 64, 44, 60, 50};
data = repmat({''}, 3, 5);
t = uitable(fig, 'Data', data, 'Position', [16 40 312 140], ...
  'ColumnName', {'Sel', 'Name', 'Folder', 'Type', 'State'}, ...
  'ColumnWidth', widths);
drawnow();

assert_isequal(t.ColumnWidth, widths);

frame = getframe(fig);
assert_column_grid_positions(frame.cdata, 16, 40, widths);

delete(fig);
%=============================================================================
function assert_column_grid_positions(img, x0, y0, widths)
  % The data columns start after a fixed 34px row-number gutter.
  gutter = 34;
  requested = cell2mat(widths);
  separators = x0 + gutter + cumsum(requested);
  separators = separators(1:end - 1);
  for k = 1:numel(separators)
    assert_istrue(is_vertical_grid_line(img, separators(k), y0));
  end

  uniformWidth = (312 - gutter) / numel(widths);
  uniformSeparator = round(x0 + gutter + uniformWidth);
  assert_isfalse(is_vertical_grid_line(img, uniformSeparator, y0));
end
%=============================================================================
function tf = is_vertical_grid_line(img, x, y0)
  x = round(x);
  lineSample = double(img(y0 + 34:y0 + 118, x, 1));
  leftSample = double(img(y0 + 34:y0 + 118, x - 5, 1));
  rightSample = double(img(y0 + 34:y0 + 118, x + 5, 1));
  neighbor = (mean(leftSample(:)) + mean(rightSample(:))) / 2;
  tf = mean(lineSample(:)) < neighbor - 12;
end
%=============================================================================
