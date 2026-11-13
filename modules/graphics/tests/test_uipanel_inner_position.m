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
% InnerPosition = Position less BorderWidth per side; the top border lies
% inside the title band, as tall as the title font in pixels.
% Values measured at 96 dpi (a scaled display rounds them to device pixels).
f = figure('Visible', 'off', 'Position', [100 100 859 517]);
pos = [20 20 260 221];
widths = [0 1 2 5];
expected = [20 20 260 221; 21 21 258 219; 22 22 256 217; 25 25 250 211];
for k = 1:numel(widths)
  p = uipanel(f, 'Units', 'pixels', 'Position', pos, 'BorderWidth', widths(k));
  assert_isequal(p.InnerPosition, expected(k, :));
  bg = uibuttongroup(f, 'Units', 'pixels', 'Position', pos, 'BorderWidth', widths(k));
  assert_isequal(bg.InnerPosition, expected(k, :));
  delete(p);
  delete(bg);
end
%=============================================================================
% no border
p = uipanel(f, 'Units', 'pixels', 'Position', pos, 'BorderType', 'none', 'BorderWidth', 5);
assert_isequal(p.InnerPosition, [20 20 260 221]);
p.Title = 'Hello';
assert_isequal(p.InnerPosition, [20 20 260 210]);
delete(p);
%=============================================================================
% title: 8 points = 11 pixels band, holding the top border
titled = [20 20 260 210; 21 21 258 209; 22 22 256 208; 25 25 250 205];
for k = 1:numel(widths)
  p = uipanel(f, 'Units', 'pixels', 'Position', pos, 'BorderWidth', widths(k), 'Title', 'Hello');
  assert_isequal(p.InnerPosition, titled(k, :));
  delete(p);
end
p = uipanel(f, 'Units', 'pixels', 'Position', pos, 'BorderWidth', 20, 'Title', 'Hello');
assert_isequal(p.InnerPosition, [40 40 220 181]);
delete(p);
%=============================================================================
% the band follows FontSize and FontUnits, not TitlePosition
p = uipanel(f, 'Units', 'pixels', 'Position', pos, 'Title', 'Hello', 'FontSize', 20);
assert_isequal(p.InnerPosition, [21 21 258 193]);
p.TitlePosition = 'centertop';
assert_isequal(p.InnerPosition, [21 21 258 193]);
p.FontUnits = 'pixels';
p.FontSize = 12;
assert_isequal(p.InnerPosition, [21 21 258 208]);
p.Title = '';
assert_isequal(p.InnerPosition, [21 21 258 219]);
delete(p);
bg = uibuttongroup(f, 'Units', 'pixels', 'Position', [20 20 260 210], 'Title', 'Hello', ...
  'FontUnits', 'pixels', 'FontSize', 12);
assert_isequal(bg.InnerPosition, [21 21 258 197]);
delete(bg);
%=============================================================================
% default panel filling the figure
p = uipanel(f);
assert_isapprox(p.InnerPosition, [1/859 1/517 857/859 515/517], 1e-12);
p.Units = 'pixels';
assert_isequal(p.Position, [1 1 859 517]);
assert_isequal(p.InnerPosition, [2 2 857 515]);
p.Title = 'Hello';
assert_isequal(p.InnerPosition, [2 2 857 505]);
close(f);
%=============================================================================
% same rule in a uifigure
uf = uifigure('Visible', 'off', 'Position', [100 100 600 500]);
p = uipanel(uf);
assert_isequal(p.Position, [20 20 260 221]);
assert_isequal(p.InnerPosition, [21 21 258 219]);
bg = uibuttongroup(uf);
assert_isequal(bg.Position, [20 20 260 210]);
assert_isequal(bg.InnerPosition, [21 21 258 208]);
close(uf);
%=============================================================================
