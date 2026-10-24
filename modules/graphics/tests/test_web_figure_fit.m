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
f = figure('Visible', 'off', 'Position', [0 0 1000 800]);
a1 = subplot(2, 2, 1); plot(1:5);
a2 = subplot(2, 2, 2); plot(5:-1:1);
a3 = subplot(2, 2, 3); surf(peaks(15));
a4 = subplot(2, 2, 4); plot3([0 1 2], [0 1 0], [0 1 2]);
drawnow();
fh = __web_graphics_handle__(f, 'number');
ah = __web_graphics_handle__(a3, 'number');
__web_figure_size__(fh, 1600, 1200);
drawnow();
__web_display_list__(f);
largeTickCount = numel(a1.XTick);
% Opening the inspector changes the drawing size without editing figure Position.
__web_figure_size__(fh, 680, 446);
resized = jsondecode(__web_display_list__(f));
asserts.istrue(largeTickCount > numel(a1.XTick));
__web_figure_action__(fh, jsonencode(struct('action', 'state')));
afterRead = jsondecode(__web_display_list__(f));
asserts.isequal(afterRead.primitives, resized.primitives);
ticks = {a1.XTick, a1.YTick, a2.XTick, a2.YTick};
slots = [a1.Position; a2.Position; a3.Position; a4.Position];
fitted = jsondecode(__web_figure_action__(fh, jsonencode(struct('action', 'fit', 'axesHandle', ah))));
asserts.isequal([fitted.displayList.width fitted.displayList.height], [680 446]);
asserts.isequal({a1.XTick, a1.YTick, a2.XTick, a2.YTick}, ticks);
asserts.isequal([a1.Position; a2.Position; a3.Position; a4.Position], slots);
asserts.isequal(f.Position, [0 0 1000 800]);
for projection = {'orthographic', 'perspective'}
  set(a3, 'Projection', projection{1});
  view(a3, -52, 20);
  drawnow();
  reference = [a3.CameraPosition a3.CameraTarget a3.CameraUpVector a3.CameraViewAngle];
  __web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, ...
    'dxFrac', 0.2, 'dyFrac', 0.1)));
  __web_figure_action__(fh, jsonencode(struct('action', 'fit', 'axesHandle', ah)));
  asserts.isapprox([a3.CameraPosition a3.CameraTarget a3.CameraUpVector a3.CameraViewAngle], reference, 1e-8);
  asserts.isequal(a3.View, [-52 20]);
  asserts.isequal(a3.CameraTargetMode, 'auto');
  asserts.isequal(a3.CameraPositionMode, 'auto');
  asserts.isequal([a1.Position; a2.Position; a3.Position; a4.Position], slots);
end
close(f);
