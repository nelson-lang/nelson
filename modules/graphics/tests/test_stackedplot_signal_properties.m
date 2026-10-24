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
t = 0:0.01:10;
y1 = sin(2 * pi * 0.5 * t);
y2 = cos(2 * pi * 0.3 * t) .* exp(-0.1 * t);
y3 = sin(2 * pi * 0.8 * t) + 0.3 * sin(2 * pi * 3 * t);
%=============================================================================
s = stackedplot(t, [y1' y2' y3'], ...
  'DisplayLabels', {'Signal A', 'Signal B', 'Signal C'}, ...
  'Title', 'Stacked Signal Comparison');
s.LineWidth = 1.5;
s.GridVisible = 'on';
s.FontSize = 11;
didError = false;
try
  s.AxesProperties(1).LineProperties.Color = [0 0.45 0.74];
catch
  didError = true;
end
asserts.istrue(didError);
xlabel('Time (s)');
asserts.isequal(s.Children(1).Children(end).LineWidth, 1.5);
asserts.isequal(char(s.Children(1).XGrid), 'on');
asserts.isequal(s.Children(1).FontSize, 11);
asserts.isequal(s.Children(1).Children(end).Color, [0.0660 0.4430 0.7450]);
asserts.isequal(s.Children(2).Children(end).Color, [0.0660 0.4430 0.7450]);
asserts.isequal(s.Children(3).Children(end).Color, [0.0660 0.4430 0.7450]);
asserts.istrue(s.Children(1).Position(1) >= 0.13);
asserts.istrue(s.Children(1).Children(end - 1).Position(1) < 0);
currentAxes = gca();
asserts.isequal(currentAxes.XLabel.String, 'Time (s)');
close all;
%=============================================================================
