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
ar = annotation('arrow', 'Color', 'red', 'LineStyle', '--', 'HeadStyle', 'plain');
tb = annotation('textbox', [0.2 0.2 0.3 0.2], 'String', 'label', ...
  'FitBoxToText', 'off', 'BackgroundColor', '#336699', 'FaceAlpha', 0.5, ...
  'Rotation', 10, 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle');
%=============================================================================
u = annotation('rectangle', [0.1 0.2 0.3 0.4]);
originalPosition = u.Position;
u.Units = 'pixels';
asserts.isequal(u.Units, 'pixels');
u.Units = 'normalized';
asserts.isapprox(u.Position, originalPosition, 1e-12);
asserts.checkerror('set(u, ''Units'', ''data'');', graphicsPropertyErrorMessage('Units', 'Nelson:graphics:illegalSelection'));
set(u, 'Units', 'pixels', 'Position', [20 30 40 50]);
asserts.isequal(u.Units, 'pixels');
asserts.isequal(u.Position, [20 30 40 50]);
asserts.checkerror('set(h, ''LineWidth'', 0);', graphicsPropertyErrorMessage('LineWidth', 'Nelson:annotation:expectedPositiveScalar'));
asserts.checkerror('set(ar, ''HeadLength'', -1);', graphicsPropertyErrorMessage('HeadLength', 'Nelson:annotation:expectedPositiveScalar'));
asserts.checkerror('set(tb, ''Margin'', -1);', graphicsPropertyErrorMessage('Margin', 'Nelson:annotation:expectedNonnegativeScalar'));
%=============================================================================
