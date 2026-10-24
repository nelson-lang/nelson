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
f = figure();
Y = linspace(-2 * pi, 2 * pi, 50);
h = stem(Y);
asserts.istrue(isgraphics(h, 'stem'));
asserts.isequal(h.Type, 'stem');
asserts.isequal(h.XData, 1:50);
asserts.isequal(h.YData, Y);
asserts.isequal(h.XDataMode, 'auto');
asserts.isequal(h.YDataMode, 'manual');
asserts.isequal(h.MarkerFaceColor, 'none');
asserts.isequal(h.MarkerEdgeColor, 'auto');
asserts.isequal(h.UserData, []);
markerLine = h.RendererState.MarkerLine;
asserts.isequal(markerLine.XData(:), (1:50)');
asserts.isapprox(gca().XLim, [0.02 50.98], 1e-12);
close all;
%=============================================================================
