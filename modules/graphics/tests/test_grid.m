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
x = linspace(0, 20);
y = cos(x);
plot(x, y)
ax = gca();
assert_isequal(ax.XGrid, 'off');
grid on
assert_isequal(ax.XGrid, 'on');
%=============================================================================
f = figure();
x = linspace(0, 10);
y = sin(x);
plot(x, y);
ax = gca();
assert_isequal(ax.XMinorGrid, 'off');
grid on
grid minor
assert_isequal(ax.XMinorGrid, 'on');
grid minor
assert_isequal(ax.XMinorGrid, 'off');
%=============================================================================
