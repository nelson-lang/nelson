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
f = figure('Visible', 'off');
xr = randn(200, 1);
yr = 0.5 * xr + randn(200, 1);
h = scatterhistogram(f, xr, yr, 'NumBins', 20);
asserts.istrue(isgraphics(h, 'scatterhistogram'));
asserts.isequal(h.NumBins, [20; 20]);
state = h.UserData;
asserts.isequal(state.Scatter.Parent, state.ScatterAxes);
asserts.isequal(state.XHistogram.Parent, state.XHistogramAxes);
asserts.isequal(state.YHistogram.Parent, state.YHistogramAxes);
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
close all
%=============================================================================
