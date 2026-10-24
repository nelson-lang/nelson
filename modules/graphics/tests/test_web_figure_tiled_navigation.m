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
f = figure('Visible', 'off', 'Position', [0 0 900 700]);
t = tiledlayout(2, 2);
a1 = nexttile; plot(1:5);
a2 = nexttile; plot(5:-1:1);
a3 = nexttile([1 2]); surf(peaks(70)); title('Dense tiled surface');
drawnow();
fh = __web_graphics_handle__(f, 'number');
ah = __web_graphics_handle__(a3, 'number');
frame = jsondecode(__web_display_list__(f));
asserts.isequal(numel(frame.rotatableAxes), 3);
slots = [a1.Position; a2.Position; a3.Position];
untouched = [a1.XLim a1.YLim; a2.XLim a2.YLim];
for projection = {'orthographic', 'perspective'}
  set(a3, 'Projection', projection{1}, 'XDir', 'reverse', 'YDir', 'reverse');
  drawnow();
  original = [a3.CameraPosition a3.CameraTarget a3.CameraViewAngle];
  __web_figure_action__(fh, jsonencode(struct('action', 'reference', 'axesHandle', ah)));
  __web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, 'dxFrac', 0.15, 'dyFrac', -0.1)));
  __web_figure_action__(fh, jsonencode(struct('action', 'zoom', 'axesHandle', ah, 'factor', 1.5)));
  __web_figure_action__(fh, jsonencode(struct('action', 'view', 'axesHandle', ah, 'azimuth', 45, 'elevation', 35, 'resetCamera', true)));
  __web_figure_action__(fh, jsonencode(struct('action', 'fit', 'axesHandle', ah)));
  asserts.isequal(a3.View, [45 35]);
  __web_figure_action__(fh, jsonencode(struct('action', 'restore', 'axesHandle', ah)));
  asserts.isapprox([a3.CameraPosition a3.CameraTarget a3.CameraViewAngle], original, 1e-8);
  asserts.isequal([a1.Position; a2.Position; a3.Position], slots);
  asserts.isequal([a1.XLim a1.YLim; a2.XLim a2.YLim], untouched);
  frame = jsondecode(__web_display_list__(f));
  asserts.isequal(numel(frame.rotatableAxes), 3);
  asserts.istrue(numel(frame.primitives) > 0);
end
asserts.isequal(t.GridSize, [2 2]);
close(f);
