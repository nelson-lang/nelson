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
f = figure();
plot((1:10).^2);
t = title('Case number # 42', 'Color', 'm');
assert_isequal(t.Type, 'text');
assert_isequal(t.String, 'Case number # 42');
assert_isequal(t.Color, [1 0 1]);
%=============================================================================
f = figure();
ax = gca();
ax.Visible = 'off';
t = title('Text not visible', 'Color', 'm');
assert_isequal(t.Visible, 'off');
ax.Visible = 'on';
assert_isequal(t.Visible, 'on');
t.Visible = 'off';
assert_isequal(t.Visible, 'off');
%=============================================================================
f = figure();
ax = gca();
ax.Visible = 'off';
t = title('Text not visible', 'Color', 'm', 'Visible', 'on');  
assert_isequal(t.Visible, 'on');
ax.Visible = 'off';
assert_isequal(t.Visible, 'off');
%=============================================================================
f = figure();
ax = gca();
t1 = title(ax, 'First title');
t2 = title(ax, 'Second title');
assert_isfalse(isgraphics(t1));
assert_istrue(isgraphics(t2));
assert_isequal(ax.Title, t2);
%=============================================================================
x1 = xlabel(ax, 'First X label');
x2 = xlabel(ax, 'Second X label');
assert_isfalse(isgraphics(x1));
assert_istrue(isgraphics(x2));
assert_isequal(ax.XLabel, x2);
%=============================================================================
y1 = ylabel(ax, 'First Y label');
y2 = ylabel(ax, 'Second Y label');
assert_isfalse(isgraphics(y1));
assert_istrue(isgraphics(y2));
assert_isequal(ax.YLabel, y2);
%=============================================================================
z1 = zlabel(ax, 'First Z label');
z2 = zlabel(ax, 'Second Z label');
assert_isfalse(isgraphics(z1));
assert_istrue(isgraphics(z2));
assert_isequal(ax.ZLabel, z2);
%=============================================================================
