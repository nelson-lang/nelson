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
% text Extent: read-only box [left bottom width height] of the text (without
% margin) in the text Units; it follows Position, alignment and the axes limits.
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f);
t = text(ax, 0.5, 0.5, 'hi');
e = t.Extent;
assert_istrue(abs(e(1) - 0.5) < 1e-6);
assert_istrue(abs(e(2) + e(4) / 2 - 0.5) < 0.005);
assert_istrue(e(3) > 0.005 && e(3) < 0.03);
assert_istrue(e(4) > 0.015 && e(4) < 0.045);
%=============================================================================
t.HorizontalAlignment = 'right';
e2 = t.Extent;
assert_istrue(abs(e2(1) + e2(3) - 0.5) < 0.002);
t.HorizontalAlignment = 'left';
t.VerticalAlignment = 'bottom';
e3 = t.Extent;
assert_istrue(abs(e3(2) - 0.5) < 0.005);
t.VerticalAlignment = 'middle';
%=============================================================================
% Same text on twice larger limits: the extent scales with the data units.
ax.XLim = [0 2];
ax.YLim = [0 2];
e4 = t.Extent;
assert_istrue(abs(e4(3) / e(3) - 2) < 0.2);
assert_istrue(abs(e4(4) / e(4) - 2) < 0.2);
ax.XLim = [0 1];
ax.YLim = [0 1];
%=============================================================================
% A Units change converts Position (pixels start at 1); Extent follows.
ax.Units = 'pixels';
axesPixels = ax.Position;
ax.Units = 'normalized';
t.Units = 'pixels';
p = t.Position;
assert_istrue(abs(p(1) - 1 - axesPixels(3) / 2) < 1);
assert_istrue(abs(p(2) - 1 - axesPixels(4) / 2) < 1);
ep = t.Extent;
assert_istrue(abs(ep(1) - p(1)) < 1e-6);
assert_istrue(ep(3) > 5 && ep(3) < 25);
assert_istrue(ep(4) > 8 && ep(4) < 25);
t.Units = 'points';
q = t.Position;
assert_istrue(max(abs(q(1:2) - (p(1:2) - 1) * 0.75)) < 1e-6);
% Conversions through data units are exact within the pixel snapping of the plot box.
t.Units = 'normalized';
assert_istrue(max(abs(t.Position(1:2) - [0.5 0.5])) < 2e-3);
en = t.Extent;
assert_istrue(abs(en(1) - t.Position(1)) < 1e-6);
t.Units = 'data';
assert_istrue(max(abs(t.Position - [0.5 0.5 0])) < 2e-3);
ax.XLim = [0 2];
t.Units = 'normalized';
assert_istrue(max(abs(t.Position(1:2) - [0.25 0.5])) < 2e-3);
t.Units = 'data';
t2 = text(ax, 1, 1, 'z', 'Units', 'normalized');
assert_isequal(t2.Position, [1 1 0]);
t2.Units = 'data';
assert_istrue(max(abs(t2.Position - [2 1 0])) < 4e-3);
ax.XLim = [0 1];
%=============================================================================
failed = false;
try
  t.Extent = [0 0 1 1];
catch
  failed = true;
end
assert_istrue(failed);
delete(f);
%=============================================================================
