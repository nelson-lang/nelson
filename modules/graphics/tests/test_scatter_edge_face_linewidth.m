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
rng('default') % For reproducibility
figure();
theta = linspace(0, 2 * pi, 300);
x = sin(theta) + 0.75 * rand(1, 300);
y = cos(theta) + 0.75 * rand(1, 300);
sz = 40;
s = scatter(x, y, sz, 'MarkerEdgeColor', [0 .5 .5], 'MarkerFaceColor', [0 .7 .7], 'LineWidth', 1.5);
asserts.isequal(s.MarkerEdgeColor, [0 0.5000 0.5000]);
asserts.isequal(s.MarkerFaceColor, [0 0.7000 0.7000]);
asserts.isequal(s.LineWidth, 1.5000);
asserts.isequal(s.SizeData, 40);
asserts.isequal(s.CData, [0.0660 0.4430 0.7450]);
%=============================================================================
close all
%=============================================================================
