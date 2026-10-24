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
f = figure('Visible', 'off');
plot(1:10);
%=============================================================================
tb = annotation('textbox', [0.2 0.2 0.3 0.2], 'String', 'label', ...
  'FitBoxToText', 'off', 'BackgroundColor', '#336699', 'FaceAlpha', 0.5, ...
  'Rotation', 10, 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle');
asserts.isequal(tb.Type, 'textboxshape');
asserts.isequal(char(tb.FitBoxToText), 'off');
asserts.isapprox(tb.BackgroundColor, [0.2 0.4 0.6], 1e-7);
asserts.isequal(tb.FaceAlpha, 0.5);
asserts.isequal(tb.Rotation, 10);
asserts.isequal(tb.HorizontalAlignment, 'center');
asserts.isequal(tb.VerticalAlignment, 'middle');
asserts.isapprox(tb.EdgeColor, [33 33 33] / 255, 1e-12);
asserts.isequal(tb.Margin, 5);
tbDefault = annotation('textbox', [0.2 0.2 0.3 0.2], 'String', 'fixed box');
asserts.isequal(char(tbDefault.FitBoxToText), 'off');
tbFit = annotation('textbox', [0.2 0.2 0.3 0.2], 'String', 'fit box', ...
  'FitBoxToText', 'on');
asserts.isequal(char(tbFit.FitBoxToText), 'on');
%=============================================================================
r = annotation('rectangle', [0.3 0.3 0.2 0.2], 'FaceColor', 'blue', ...
  'FaceAlpha', 0.25, 'Rotation', 30);
asserts.isequal(r.Type, 'rectangleshape');
asserts.isequal(r.FaceColor, [0 0 1]);
asserts.isequal(r.FaceAlpha, 0.25);
asserts.isequal(r.Rotation, 30);
%=============================================================================
e = annotation('ellipse', [0.5 0.5 0.2 0.1], 'FaceColor', 'none', ...
  'LineStyle', ':', 'Rotation', 45);
asserts.isequal(e.Type, 'ellipseshape');
asserts.isequal(e.FaceColor, 'none');
asserts.isequal(e.LineStyle, ':');
asserts.isequal(e.Rotation, 45);
asserts.isfalse(isprop(e, 'FaceAlpha'));
%=============================================================================
