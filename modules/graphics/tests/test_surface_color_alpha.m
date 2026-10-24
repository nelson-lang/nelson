%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
[X, Y] = meshgrid(1:0.5:10, 1:20);
Z = sin(X) + cos(Y);
C = X .* Y;
surface(X, Y, Z, C);
colorbar
view(3)
%=============================================================================
f = figure();
ax = gca();
[X, Y] = meshgrid(-5:.5:5);
Z = Y .* sin(X) - X .* cos(Y);
s = surface(X, Y, Z, 'FaceAlpha', 0.5, 'EdgeColor', 'none');
asserts.isequal(s.EdgeColor, 'none');
view(3)
%=============================================================================
f = figure('Visible', 'off');
[X, Y] = meshgrid(1:3, 1:3);
Z = X + Y;
s = surf(X, Y, Z, 'EdgeColor', 'None');
asserts.isequal(s.EdgeColor, 'none');
close(f);
close all;
%=============================================================================
