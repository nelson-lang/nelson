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
x = 0:pi/100:2*pi;
y = sin(x);
plot(x, y);
xlabel('X label');
ax = gca();
assert_istrue(isgraphics(ax.XLabel, 'text'));
assert_isequal(ax.XLabel.String, 'X label');
%=============================================================================
f = figure();
x = 0:pi/100:2*pi;
y = sin(x);
plot(x, y);
ylabel('Y label');
ax = gca();
assert_istrue(isgraphics(ax.YLabel, 'text'));
assert_isequal(ax.YLabel.String, 'Y label');
%=============================================================================
f = figure();
x = 0:pi/100:2*pi;
y = sin(x);
plot(x, y);
zlabel('Z label');
ax = gca();
assert_istrue(isgraphics(ax.ZLabel, 'text'));
assert_isequal(ax.ZLabel.String, 'Z label');
%=============================================================================
f = figure();
x = 0:pi/100:2*pi;
y = sin(x);
plot(x, y);
ax = gca();
xlabel(ax, 'X label');
assert_istrue(isgraphics(ax.XLabel, 'text'));
assert_isequal(ax.XLabel.String, 'X label');
%=============================================================================
