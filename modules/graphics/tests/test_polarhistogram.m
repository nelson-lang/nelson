%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
f = figure('Visible', 'off');
theta = [0.1 0.2 1.8 3.2];
h = polarhistogram(theta, 2, 'FaceAlpha', 0.4);
asserts.istrue(isgraphics(h, 'histogram'));
asserts.isequal(h.Type, 'histogram');
asserts.isequal(char(h.Visible), 'on');
asserts.isequal(char(h.HandleVisibility), 'on');
asserts.isequal(h.Parent.Type, 'polaraxes');
asserts.isequal(h.Values, [3 1]);
asserts.isequal(h.FaceColor, 'auto');
asserts.isapprox(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-15);
state = h.UserData;
asserts.isequal(state.ChartType, 'polarhistogram');
asserts.isequal(state.Values, [3 1]);
asserts.isapprox(state.BinEdges, [0 5 * pi / 8 5 * pi / 4], 1e-4);
asserts.isapprox(h.BinEdges, [0 5 * pi / 8 5 * pi / 4], 1e-4);
asserts.istrue(isgraphics(state.LimitHandle, 'line'));
asserts.isequal(numel(state.Patches), 2);
asserts.istrue(isgraphics(state.Patches(1), 'patch'));
asserts.isequal(h.FaceAlpha, 0.4);
asserts.isapprox(state.Patches(1).FaceColor, h.Parent.ColorOrder(1, :), 1e-15);
asserts.isapprox(state.Patches(1).EdgeColor, h.EdgeColor, 1e-15);
asserts.isequal(state.Patches(1).FaceLighting, 'none');
asserts.isequal(state.Patches(1).EdgeLighting, 'none');
drawnow();
frame = getframe(f);
render_assert_not_blank(frame.cdata);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = polarhistogram(theta, 'BinEdges', [0 pi 2 * pi], 'Normalization', 'probability');
asserts.isapprox(sum(h.UserData.Values), 1, 1e-12);
asserts.isapprox(sum(h.Values), 1, 1e-12);
close(f);
%=============================================================================
asserts.checkerror('polarhistogram();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('polarhistogram({1, 2});', getString(message('Nelson:validators:mustBeNumeric', 'Angle data')));
asserts.checkerror('polarhistogram([1 2], ''NumBins'', 0);', getString(message('Nelson:graphics:mustBePositiveScalar', 'NumBins')));
asserts.checkerror('polarhistogram([1 2], ''Normalization'', ''pdf'');', getString(message('Nelson:polarhistogram:invalidNormalization')));
%=============================================================================
