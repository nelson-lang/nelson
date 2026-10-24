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
[X, Y] = meshgrid(1:0.5:10, 1:20);
Z = sin(X) + cos(Y);
h = surface(X, Y, Z);
asserts.isapprox(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
close all;
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
surface(ax, peaks(50));
view(ax, 2);
asserts.isapprox(ax.ZLim, [-6.5258916788603045 8.04358645092234], 1e-12);
asserts.isequal(ax.ZTick, [-5 0 5]);
close(f);
%=============================================================================
% an empty positional C (surf(X,Y,Z,[])) colours by Z, like surf(X,Y,Z)
f = figure('Visible', 'off');
[X, Y] = meshgrid(1:5, 1:5);
Z = X + Y;
h = surf(X, Y, Z, [], 'FaceColor', '#aaa', 'EdgeColor', '#333');
asserts.isequal(size(get(h, 'CData')), [5 5]);
close(f);
%=============================================================================
