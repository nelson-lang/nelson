%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
%=============================================================================
f = figure();
s = scatter(1, 1);
asserts.isequal(s.Type, 'scatter');
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 0.5000);
asserts.isequal(s.XData, 1);
asserts.isequal(s.YData, 1);
asserts.isequal(s.SizeData, 36);
asserts.isequal(s.CData, [0.0660 0.4430 0.7450]);
%=============================================================================
rng('default') % For reproducibility
figure();
x = randn(100, 1);
y = randn(100, 1);
scatter(x, y)
%=============================================================================
f = figure();
theta = linspace(0, 1, 500);
x = exp(theta) .* sin(100 * theta);
y = exp(theta) .* cos(100 * theta);
s = scatter(x, y);
%=============================================================================
f = figure();
theta = linspace(0, 1, 500);
x = exp(theta) .* sin(100 * theta);
y = exp(theta) .* cos(100 * theta);
s = scatter(x, y, 'filled');
asserts.isequal(s.Marker, 'o');
asserts.isequal(s.MarkerEdgeColor, 'none');
asserts.isequal(s.MarkerFaceColor, 'flat');
%=============================================================================
close all
%=============================================================================
