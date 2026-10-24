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
t = 0:0.01:10;
y1 = sin(2 * pi * 0.5 * t);
y2 = cos(2 * pi * 0.3 * t) .* exp(-0.1 * t);
y3 = sin(2 * pi * 0.8 * t) + 0.3 * sin(2 * pi * 3 * t);
s = stackedplot(t, [y1' y2' y3'], ...
  'DisplayLabels', {'Signal A', 'Signal B', 'Signal C'}, ...
  'Title', 'Stacked Signal Comparison');
s.LineWidth = 1.5;
s.LineProperties(1).Color = [0 0.45 0.74];
s.LineProperties(2).Color = [0.85 0.33 0.10];
s.LineProperties(3).Color = [0.47 0.67 0.19];
asserts.isequal(s.Children(1).Children(end).LineWidth, 1.5);
asserts.isequal(s.Children(1).Children(end).Color, [0 0.45 0.74]);
asserts.isequal(s.Children(2).Children(end).Color, [0.85 0.33 0.10]);
asserts.isequal(s.Children(3).Children(end).Color, [0.47 0.67 0.19]);
close all;
%=============================================================================
