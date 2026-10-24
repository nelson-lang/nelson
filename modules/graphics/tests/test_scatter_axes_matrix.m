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
x = linspace(0, 3 * pi, 200);
y = cos(x) + rand(1, 200);
% Top plot
ax1 = subplot(2, 1, 1);
s1 = scatter(ax1, x, y);
asserts.isequal(s1.Marker, 'o');
asserts.isequal(s1.MarkerEdgeColor, 'flat');
asserts.isequal(s1.MarkerFaceColor, 'none');
asserts.isequal(s1.LineWidth, 0.5000);
asserts.isequal(s1.XData, x);
asserts.isequal(s1.YData, y);
asserts.isequal(s1.SizeData, 36);
asserts.isequal(s1.CData, [0.0660 0.4430 0.7450]);
% Bottom plot
ax2 = subplot(2, 1, 2);
s2 = scatter(ax2, x, y, 'filled', 'd');
asserts.isequal(s2.Marker, 'diamond');
asserts.isequal(s2.MarkerEdgeColor, 'none');
asserts.isequal(s2.MarkerFaceColor, 'flat');
asserts.isequal(s2.LineWidth, 0.5000);
asserts.isequal(s2.XData, x);
asserts.isequal(s2.YData, y);
asserts.isequal(s2.SizeData, 36);
asserts.isequal(s2.CData, [0.0660 0.4430 0.7450]);
%=============================================================================
close all
%=============================================================================
