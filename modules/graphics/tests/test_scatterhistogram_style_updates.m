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
[x, y] = graphics_scatterhistogram_fixture();
f = figure('Visible', 'off');
h = scatterhistogram(f, x, y, 'HistogramDisplayStyle', 'bar', 'Color', [1 0 0], ...
  'NumBins', [4 5], 'MarkerFilled', 'off', 'LineWidth', 1.5);
asserts.isequal(h.Parent, f);
state = h.UserData;
asserts.isequal(state.XHistogram.DisplayStyle, 'bar');
asserts.isequal(state.YHistogram.DisplayStyle, 'bar');
asserts.isequal(state.Scatter.CData, [1 0 0]);
asserts.isequal(state.Scatter.MarkerFaceColor, 'none');
asserts.isequal(state.XHistogram.LineWidth, 1.5);
asserts.isequal(h.HistogramDisplayStyle, 'bar');
asserts.isequal(h.Color, [1 0 0]);
asserts.isequal(h.NumBins, [4; 5]);
asserts.isequal(char(h.MarkerFilled), 'off');
h.Color = [0 1 0];
h.MarkerStyle = 'd';
h.MarkerSize = 12;
h.MarkerFilled = 'on';
h.NumBins = [2; 3];
h.XLimits = [0 7];
h.YLimits = [1 6];
asserts.isequal(state.Scatter.CData, [0 1 0]);
asserts.isequal(state.Scatter.Marker, 'diamond');
asserts.isequal(state.Scatter.SizeData, 12);
asserts.isequal(state.Scatter.MarkerFaceColor, 'flat');
asserts.isequal(state.XHistogram.NumBins, 2);
asserts.isequal(state.YHistogram.NumBins, 3);
asserts.isequal(state.ScatterAxes.XLim, [0 7]);
asserts.isequal(state.YHistogramAxes.YLim, [1 6]);
close(f);
close all
%=============================================================================
