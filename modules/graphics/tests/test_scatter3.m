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
close all
%=============================================================================
f = figure();
s = scatter3(1, 1, 1);
asserts.isequal(s.Type, 'scatter');
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 0.5000);
asserts.isequal(s.XData, 1);
asserts.isequal(s.YData, 1);
asserts.isequal(s.ZData, 1);
asserts.isequal(s.SizeData, 36);
asserts.isequal(s.CData, [0.0660 0.4430 0.7450]);
%=============================================================================
f = figure('Visible', 'off');
rng(0);
n = 100;
x = randn(n, 1);
y = randn(n, 1);
z = randn(n, 1);
c = z;
sz = 20 + 50 * sqrt(x .^ 2 + y .^ 2 + z .^ 2);
scatter3(x, y, z, sz, c, 'filled');
% Add labels and title
xlabel('X Axis');
ylabel('Y Axis');
zlabel('Z Axis');
title('3D Scatter Plot Demo');
grid on;
axis equal;
view(-66.5, 12);
ax = gca();
asserts.isequal(ax.XTick, [-2 -1 0 1 2 3]);
asserts.isequal(ax.YTick, [-2 -1 0 1 2]);
asserts.isequal(ax.ZTick, [-3 -2.5 -2 -1.5 -1 -0.5 0 0.5 1 1.5 2]); %=============================================================================
