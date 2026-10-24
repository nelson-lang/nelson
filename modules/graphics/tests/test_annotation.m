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
h = annotation('line', [0.1 0.2], [0.3 0.4]);
asserts.istrue(isgraphics(h));
asserts.isequal(h.Type, 'lineshape');
asserts.isequal(h.X, [0.1 0.2]);
asserts.isequal(h.Y, [0.3 0.4]);
asserts.isapprox(h.Position, [0.1 0.3 0.1 0.1], 1e-12);
h.Position = [0.2 0.3 0.4 0.5];
asserts.isapprox(h.X, [0.2 0.6], 1e-12);
asserts.isapprox(h.Y, [0.3 0.8], 1e-12);
h.X = [0.4 0.7];
h.Y = [0.1 0.5];
asserts.isapprox(h.Position, [0.4 0.1 0.3 0.4], 1e-12);
asserts.isapprox(h.Color, [33 33 33] / 255, 1e-12);
asserts.isequal(h.LineStyle, '-');
asserts.isequal(h.LineWidth, 0.5);
%=============================================================================
tb = annotation('textbox', [0.2 0.2 0.3 0.2], 'String', 'label', ...
  'FitBoxToText', 'off', 'BackgroundColor', '#336699', 'FaceAlpha', 0.5, ...
  'Rotation', 10, 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle');
asserts.isequal(tb.Type, 'textboxshape');
%=============================================================================
c = copyobj(h, f);
asserts.isequal(c.Type, 'lineshape');
asserts.isequal(c.X, h.X);
asserts.isequal(c.Y, h.Y);
asserts.isequal(c.Parent, f);
foundTextBoxes = findobj(f, 'Type', 'textboxshape');
asserts.istrue(any(foundTextBoxes == tb));
delete(c);
asserts.isfalse(isgraphics(c));
%=============================================================================
