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
function points = coloredPoints(f, color)
  frame = jsondecode(__web_display_list__(f));
  points = '';
  for k = 1:numel(frame.primitives)
    primitive = frame.primitives{k};
    if isfield(primitive, 'stroke') && strcmp(primitive.stroke, color) ...
        && isfield(primitive, 'pointsB64')
      points = [points primitive.pointsB64];
    end
  end
  asserts.istrue(~isempty(points));
end
f = figure('Visible', 'off');
a1 = subplot(2, 2, 1); plot(1:5, 'Color', [1 0 1]);
a2 = subplot(2, 2, 2); plot3([0 1 2], [0 1 0], [0 1 2], 'Color', [0 1 1]);
a3 = subplot(2, 2, 3); surf(peaks(5));
a4 = subplot(2, 2, 4); plot(5:-1:1);
% A small rotation switches an originally 2D curve to camera-based panning.
view(a1, 3.5, 87);
drawnow();
fh = __web_graphics_handle__(f, 'number');
ah = __web_graphics_handle__(a1, 'number');
for angleMode = {'auto', 'manual'}
  set(a1, 'CameraViewAngleMode', angleMode{1});
  drawnow();
  before = coloredPoints(f, 'rgba(255,0,255,1.000000)');
  other = coloredPoints(f, 'rgba(0,255,255,1.000000)');
  position = a1.CameraPosition; target = a1.CameraTarget;
  __web_figure_action__(fh, jsonencode(struct('action', 'pan', ...
    'axesHandle', ah, 'dxFrac', 0.1, 'dyFrac', 0.04)));
  asserts.istrue(~isequal(coloredPoints(f, 'rgba(255,0,255,1.000000)'), before));
  asserts.isequal(coloredPoints(f, 'rgba(0,255,255,1.000000)'), other);
  asserts.isapprox(a1.CameraPosition - position, a1.CameraTarget - target, 1e-8);
  asserts.isequal(a1.CameraViewAngleMode, angleMode{1});
end
close(f);
