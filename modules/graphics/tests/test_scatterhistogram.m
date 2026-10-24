%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
f = figure('Visible', 'off');
x = [1 2 3 4 5 6];
y = [2 3 2 4 5 4];
h = scatterhistogram(f, x, y, 'NumBins', 3, 'MarkerStyle', 's', 'MarkerSize', 25, ...
  'Title', 'Samples', 'XLabel', 'x', 'YLabel', 'y');
asserts.istrue(isgraphics(h, 'scatterhistogram'));
asserts.isequal(h.Type, 'scatterhistogram');
asserts.isequal(h.Parent, f);
asserts.isequal(h.XData(:), x(:));
asserts.isequal(h.YData(:), y(:));
asserts.isequal(h.NumBins, [3; 3]);
asserts.isequal(h.MarkerStyle, 's');
asserts.isequal(h.MarkerSize, 25);
asserts.isapprox(h.Color, [0.066 0.443 0.745], 1e-12);
asserts.isequal(h.HistogramDisplayStyle, 'stairs');
asserts.isequal(h.ScatterPlotLocation, 'SouthWest');
asserts.isequal(h.ScatterPlotProportion, 0.75);
asserts.isequal(h.XHistogramDirection, 'up');
asserts.isequal(h.YHistogramDirection, 'right');
asserts.isequal(char(h.MarkerFilled), 'on');
asserts.isequal(length(h.Children), 3);
state = h.UserData;
asserts.isequal(state.ChartType, 'scatterhistogram');
asserts.istrue(isgraphics(state.Scatter, 'scatter'));
asserts.istrue(isgraphics(state.XHistogram, 'histogram'));
asserts.istrue(isgraphics(state.YHistogram, 'histogram'));
asserts.isequal(state.Scatter.Parent, state.ScatterAxes);
asserts.isequal(state.XHistogram.Parent, state.XHistogramAxes);
asserts.isequal(state.YHistogram.Parent, state.YHistogramAxes);
asserts.isequal(state.Options.NumBins, [3; 3]);
asserts.isequal(state.Scatter.Marker, 'square');
asserts.isequal(state.Scatter.SizeData, 25);
asserts.isequal(state.Scatter.MarkerFaceColor, 'flat');
asserts.isequal(state.XHistogramAxes.XLim, state.ScatterAxes.XLim);
asserts.isequal(state.YHistogramAxes.YLim, state.ScatterAxes.YLim);
asserts.istrue(isempty(state.XHistogramAxes.YTick));
asserts.istrue(isempty(state.XHistogramAxes.YTickLabel));
asserts.istrue(isempty(state.YHistogramAxes.XTick));
asserts.istrue(isempty(state.YHistogramAxes.XTickLabel));
asserts.istrue(isempty(state.XHistogramAxes.XTickLabel));
asserts.istrue(isempty(state.YHistogramAxes.YTickLabel));
asserts.isequal(h.XLimits, state.ScatterAxes.XLim);
asserts.isequal(h.YLimits, state.ScatterAxes.YLim);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
close all
%=============================================================================
