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
x = linspace(-pi, pi);
y1 = cos(x);
plot(x, y1)
hold on
y2 = sin(x);
plot(x, y2)
hold off
%=============================================================================
ax = f.Children;
asserts.isequal(size(ax.Children), [2 1]);
asserts.isequal(ax.Children(1).Type, 'line');
asserts.isequal(ax.Children(2).Type, 'line');
%=============================================================================
