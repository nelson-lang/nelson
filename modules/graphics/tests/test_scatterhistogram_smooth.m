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
h = scatterhistogram(f, x, y, 'HistogramDisplayStyle', 'smooth');
state = h.UserData;
asserts.istrue(isgraphics(state.XHistogram, 'line'));
asserts.istrue(isgraphics(state.YHistogram, 'line'));
asserts.isequal(h.HistogramDisplayStyle, 'smooth');
close(f);
close all
%=============================================================================
