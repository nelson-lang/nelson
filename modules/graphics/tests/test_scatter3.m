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
close all
%=============================================================================
f = figure();
s = scatter3(1, 1, 1);
assert_isequal(s.Type, 'scatter');
assert_isequal(s.Marker, 'o');
assert_isequal(s.MarkerEdgeColor , 'flat');
assert_isequal(s.MarkerFaceColor , 'none');
assert_isequal(s.LineWidth , 0.5000);
assert_isequal(s.XData , 1);
assert_isequal(s.YData , 1);
assert_isequal(s.ZData , 1);
assert_isequal(s.SizeData , 36);
assert_isequal(s.CData , [0 0.4470 0.7410]);
%=============================================================================
f = figure();
n = 100;
x = randn(n,1);
y = randn(n,1);
z = randn(n,1);
c = z;
sz = 20 + 50 * sqrt(x.^2 + y.^2 + z.^2);
scatter3(x, y, z, sz, c, 'filled');
% Add labels and title
xlabel('X Axis');
ylabel('Y Axis');
zlabel('Z Axis');
title('3D Scatter Plot Demo');
grid on;
axis equal;
view(-66.5, 12);
%=============================================================================
