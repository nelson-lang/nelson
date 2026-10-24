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
[T, rowTimes, temperature, ~, rain] = graphics_stackedplot_fixture();
%=============================================================================
T2 = table(rowTimes, temperature + 1, rain + 0.1, ...
  'VariableNames', {'Time', 'Temperature', 'Rain'});
s = stackedplot(T, T2, {'Temperature', 'Rain'}, 'XVariable', 'Time');
asserts.isequal(length(s.Children), 2);
asserts.isequal(s.DisplayVariables, {'Temperature', 'Rain'});
asserts.isequal(s.Children(1).Children(end).Type, 'line');
asserts.isequal(s.Children(1).Children(end - 1).Type, 'line');
asserts.isequal(s.AxesProperties(1).LegendLabels, {'Input 1', 'Input 2'});
expectedColors = s.Children(1).ColorOrder(1:2, :);
asserts.isequal(s.Children(1).Children(end).Color, expectedColors(1, :));
asserts.isequal(s.Children(1).Children(end - 1).Color, expectedColors(2, :));
asserts.isequal(s.LineProperties(1).Color, expectedColors);
close all;
%=============================================================================
