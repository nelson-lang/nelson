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
x = rand(1, 100);
y = rand(1, 100);
ax = gca();
s = scatter(ax, x, y, 100, 'b', 'Marker', 'x', 'LineWidth', 3);
asserts.isequal(s.Marker, 'x');
asserts.isequal(s.MarkerEdgeColor, 'flat');
asserts.isequal(s.MarkerFaceColor, 'none');
asserts.isequal(s.LineWidth, 3);
asserts.isequal(s.XData, x);
asserts.isequal(s.YData, y);
asserts.isequal(s.SizeData, 100);
asserts.isequal(s.CData, [0 0 1]);
%=============================================================================
close all
%=============================================================================
