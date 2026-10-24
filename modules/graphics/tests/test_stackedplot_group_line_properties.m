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
[T] = graphics_stackedplot_fixture();
%=============================================================================
s = stackedplot(T, {{'Temperature', 'Rain'}, 'Pressure'}, 'XVariable', 'Time');
s.LineProperties(1).Color = [1 0 0; 0 0 1];
s.LineProperties(1).LineWidth = [2 3];
s.LineProperties(1).LineStyle = {'--', ':'};
s.LineProperties(1).Marker = {'o', 'x'};
s.LineProperties(1).MarkerSize = [8 9];
s.LineProperties(1).MarkerFaceColor = [0 1 0; 1 1 0];
s.LineProperties(1).MarkerEdgeColor = [0 0 0; 1 0 1];
asserts.isequal(s.Children(1).Children(end).Color, [1 0 0]);
asserts.isequal(s.Children(1).Children(end - 1).Color, [0 0 1]);
asserts.isequal(s.Children(1).Children(end).LineWidth, 2);
asserts.isequal(s.Children(1).Children(end - 1).LineWidth, 3);
asserts.isequal(s.Children(1).Children(end).LineStyle, '--');
asserts.isequal(s.Children(1).Children(end - 1).LineStyle, ':');
asserts.isequal(s.Children(1).Children(end).Marker, 'o');
asserts.isequal(s.Children(1).Children(end - 1).Marker, 'x');
asserts.isequal(s.Children(1).Children(end).MarkerSize, 8);
asserts.isequal(s.Children(1).Children(end - 1).MarkerSize, 9);
asserts.isequal(s.Children(1).Children(end).MarkerFaceColor, [0 1 0]);
asserts.isequal(s.Children(1).Children(end - 1).MarkerFaceColor, [1 1 0]);
asserts.isequal(s.Children(1).Children(end).MarkerEdgeColor, [0 0 0]);
asserts.isequal(s.Children(1).Children(end - 1).MarkerEdgeColor, [1 0 1]);
s.LineProperties(1).PlotType = {'scatter', 'plot'};
asserts.isequal(s.LineProperties(1).PlotType, {'scatter', 'plot'});
asserts.isequal(s.Children(1).Children(end).LineStyle, 'none');
asserts.isequal(s.Children(1).Children(end).Marker, 'o');
s.LineProperties(1).PlotType = {'plot', 'stairs'};
asserts.isequal(s.Children(1).Children(end).LineStyle, '--');
asserts.isequal(length(s.Children(1).Children(end - 1).XData), 11);
asserts.isequal(length(s.Children(1).Children(end - 1).YData), 11);
close all;
%=============================================================================
