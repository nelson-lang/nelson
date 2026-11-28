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
% uicontrol: InnerPosition and OuterPosition equal Position; Extent is the
% read-only [0 0 width height] of the String in the control font (plus a fixed
% padding), in the control Units, independent of Position.
%=============================================================================
f = figure('Visible', 'off');
u = uicontrol(f);
assert_isequal(u.Position, [20 20 60 20]);
assert_isequal(u.InnerPosition, [20 20 60 20]);
assert_isequal(u.OuterPosition, [20 20 60 20]);
u.Position = [50 60 200 40];
assert_isequal(u.InnerPosition, [50 60 200 40]);
assert_isequal(u.OuterPosition, [50 60 200 40]);
%=============================================================================
% a point is 96/72 pixel on most screens and one pixel on a 72 dpi screen
pixelsPerPoint = get(groot, 'ScreenPixelsPerInch') / 72;
padding = [4 6] * pixelsPerPoint;
fontPixels = round(8 * pixelsPerPoint);
u = uicontrol(f, 'Style', 'text', 'String', '');
e = u.Extent;
assert_isequal(e(1:2), [0 0]);
assert_istrue(all(abs(e(3:4) - padding) < 1));
%=============================================================================
u = uicontrol(f, 'Style', 'text', 'String', 'Hello world');
e = u.Extent;
assert_isequal(e(1:2), [0 0]);
% about 5 em wide and one line high, around the padding
textSize = e(3:4) - padding;
assert_istrue(textSize(1) > 3.5 * fontPixels && textSize(1) < 7 * fontPixels);
assert_istrue(textSize(2) > 0.8 * fontPixels && textSize(2) < 2 * fontPixels);
u.Position = [50 60 200 40];
assert_isequal(u.Extent, e);
u.String = 'Hello world, a longer string';
e2 = u.Extent;
assert_istrue(e2(3) > 1.8 * e(3));
assert_isequal(e2(4), e(4));
u.String = sprintf('two\nlines');
e3 = u.Extent;
assert_istrue(e3(4) > e(4) + 0.8 * fontPixels);
u.String = 'Hello world';
u.FontSize = 20;
e4 = u.Extent;
assert_istrue(e4(3) > 2 * e(3) && e4(4) > 1.5 * e(4));
%=============================================================================
u.FontSize = 8;
u.Units = 'normalized';
en = u.Extent;
assert_isequal(en(1:2), [0 0]);
assert_istrue(en(3) > 0 && en(3) < 0.2 && en(4) > 0 && en(4) < 0.1);
%=============================================================================
failed = false;
try
  u.Extent = [1 2 3 4];
catch
  failed = true;
end
assert_istrue(failed);
delete(f);
%=============================================================================
