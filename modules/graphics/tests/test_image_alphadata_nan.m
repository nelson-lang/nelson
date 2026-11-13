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
% Image AlphaData keeps its shape and class, and makes cells transparent
% whatever the call order (in the call, before or after a CData change).
%=============================================================================
C = magic(6);
C(2:3, 2:3) = NaN;
mask = ~isnan(C);
f = figure('Visible', 'off', 'Position', [100 100 600 600]);
ax = axes(f);
h = image(ax, magic(3));
assert_isequal(h.AlphaData, 1);
assert_isequal(class(h.AlphaData), 'double');
h.AlphaData = uint8([1 2; 3 4]);
assert_isequal(class(h.AlphaData), 'uint8');
assert_isequal(size(h.AlphaData), [2 2]);
close(f);
%=============================================================================
for order = 1:3
  f = figure('Visible', 'off', 'Position', [100 100 600 600]);
  ax = axes(f);
  switch order
    case 1
      h = imagesc(ax, C, 'AlphaData', mask);
      assert_isequal(size(h.AlphaData), [6 6]);
      assert_isequal(class(h.AlphaData), 'logical');
    case 2
      h = imagesc(ax, zeros(6));
      h.AlphaData = mask;
      h.CData = C;
    case 3
      h = imagesc(ax, C);
      h.AlphaData = mask;
  end
  assert_isequal(h.AlphaData, mask);
  ax.Color = [1 0 0];
  img = getframe(f);
  % Cell (2,2) is NaN: the red axes background shows through; cell (5,5) is opaque.
  assert_istrue(isRed(cellPixel(img.cdata, ax, 2, 2)), sprintf('order %d', order));
  assert_isfalse(isRed(cellPixel(img.cdata, ax, 5, 5)), sprintf('order %d', order));
  % Back to fully opaque: the NaN cells show the lowest colormap color again.
  h.AlphaData = 1;
  img = getframe(f);
  assert_isfalse(isRed(cellPixel(img.cdata, ax, 2, 2)));
  close(f);
end
%=============================================================================
function rgb = cellPixel(cdata, ax, row, col)
  [h, w, ~] = size(cdata);
  p = ax.Position;
  xl = ax.XLim;
  yl = ax.YLim;
  fx = p(1) + p(3) * (col - xl(1)) / (xl(2) - xl(1));
  % imagesc uses YDir reverse: row 1 is at the top.
  fy = p(2) + p(4) * (1 - (row - yl(1)) / (yl(2) - yl(1)));
  x = round(fx * w);
  y = round((1 - fy) * h);
  rgb = double(squeeze(cdata(y, x, :)))';
end
%=============================================================================
function tf = isRed(rgb)
  tf = rgb(1) > 200 && rgb(2) < 60 && rgb(3) < 60;
end
%=============================================================================
