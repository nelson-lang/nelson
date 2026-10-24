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
[T] = graphics_stackedplot_fixture();
s = stackedplot(T, {'Temperature', 'Rain'}, ...
  'XLimits', [1 3], ...
  'FontSize', 14, ...
  'MarkerFaceColor', [0 1 0], ...
  'LegendVisible', 'on', ...
  'LegendLabels', {'Temperature', 'Rain'});
asserts.isequal(s.XLimits, [1 3]);
asserts.isequal(s.Children(1).XLim, [1 3]);
asserts.isequal(s.FontSize, 14);
asserts.isequal(s.Children(1).FontSize, 14);
asserts.isequal(s.MarkerFaceColor, [0 1 0]);
asserts.isequal(s.Children(1).Children(end).MarkerFaceColor, [0 1 0]);
legendAxes = findobj(gcf(), 'Tag', 'legend');
asserts.istrue(~isempty(legendAxes));
close all;
%=============================================================================
