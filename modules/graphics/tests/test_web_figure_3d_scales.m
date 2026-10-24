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
% <--RELEASE ONLY-->
function points = tracePoints(f)
  frame = jsondecode(__web_display_list__(f));
  points = '';
  for k = 1:numel(frame.primitives)
    p = frame.primitives{k};
    if isfield(p, 'stroke') && strcmp(p.stroke, 'rgba(255,0,255,1.000000)') && isfield(p, 'pointsB64')
      points = [points p.pointsB64];
    end
  end
  asserts.istrue(~isempty(points));
end
% Compare a log-space curve to its explicitly transformed linear counterpart.
% Identical camera coordinates must produce identical projected geometry.
f = figure('Visible', 'off', 'Position', [0 0 400 300]);
a = axes('Parent', f, 'Position', [0.15 0.15 0.7 0.7]);
h = line('Parent', a, 'XData', [1 10 100 10], 'YData', [1 100 10 10], ...
  'ZData', [10 1 100 10], 'Color', [1 0 1], 'LineWidth', 200);
g = figure('Visible', 'off', 'Position', [0 0 400 300]);
b = axes('Parent', g, 'Position', a.Position);
j = line('Parent', b, 'XData', [0 1 2 1], 'YData', [0 2 1 1], ...
  'ZData', [1 0 2 1], 'Color', [1 0 1], 'LineWidth', 200);
fh = __web_graphics_handle__(f, 'number'); ah = __web_graphics_handle__(a, 'number');
gh = __web_graphics_handle__(g, 'number'); bh = __web_graphics_handle__(b, 'number');
for scales = 0:7
  for k = 1:3
    dimension = char('X' + k - 1);
    data = get(j, [dimension 'Data']);
    if bitget(scales, k)
      set(h, [dimension 'Data'], 10 .^ data);
      set(a, [dimension 'Scale'], 'log', [dimension 'Lim'], [1 100]);
    else
      set(h, [dimension 'Data'], data);
      set(a, [dimension 'Scale'], 'linear', [dimension 'Lim'], [0 2]);
    end
    set(b, [dimension 'Lim'], [0 2]);
  end
  for directions = 0:7
    for k = 1:3
      direction = 'normal';
      if bitget(directions, k), direction = 'reverse'; end
      set(a, [char('X' + k - 1) 'Dir'], direction);
      set(b, [char('X' + k - 1) 'Dir'], direction);
    end
    for projection = {'orthographic', 'perspective'}
      for cameraMode = {'auto', 'manual'}
        for ax = [a b]
          set(ax, 'Projection', projection{1}, 'View', [-37.5 30], ...
            'CameraPositionMode', 'auto', 'CameraTargetMode', 'auto', ...
            'CameraUpVectorMode', 'auto', 'CameraViewAngleMode', 'auto');
          drawnow();
          if strcmp(cameraMode{1}, 'manual')
            set(ax, 'CameraPosition', [8 -9 7], 'CameraTarget', [1 1 1], ...
              'CameraUpVector', [0 0 1], 'CameraViewAngle', 18);
          end
        end
        asserts.isequal(tracePoints(f), tracePoints(g));
        for pair = [fh gh; ah bh]
          __web_figure_action__(pair(1), jsonencode(struct('action', 'reference', 'axesHandle', pair(2))));
        end
        center = jsondecode(__web_figure_pick__(f, 200, 150));
        asserts.istrue(center.found);
        original = [a.XLim; a.YLim; a.ZLim];
        camera = a.CameraPosition; target = a.CameraTarget;
        for pair = [fh gh; ah bh]
          __web_figure_action__(pair(1), jsonencode(struct('action', 'pan', ...
            'axesHandle', pair(2), 'dxFrac', 0.08, 'dyFrac', -0.03)));
        end
        picked = jsondecode(__web_figure_pick__(f, 200, 150));
        asserts.istrue(picked.found);
        asserts.isequal(picked.index, 4);
        asserts.istrue(picked.pixel(1) > center.pixel(1));
        asserts.istrue(picked.pixel(2) > center.pixel(2));
        delta = a.CameraTarget - target;
        asserts.isapprox(a.CameraPosition - camera, delta, 1e-9);
        for k = 1:3
          limits = get(a, [char('X' + k - 1) 'Lim']);
          if bitget(scales, k)
            asserts.istrue(all(limits > 0));
            asserts.isapprox(log10(limits), log10(original(k, :)) + delta(k), 1e-8);
          else
            asserts.isapprox(limits, original(k, :) + delta(k), 1e-8);
          end
        end
        asserts.isequal(tracePoints(f), tracePoints(g));
        for pair = [fh gh; ah bh]
          __web_figure_action__(pair(1), jsonencode(struct('action', 'zoom', 'axesHandle', pair(2), 'factor', 1.25)));
          __web_figure_action__(pair(1), jsonencode(struct('action', 'view', 'axesHandle', pair(2), 'azimuth', -52, 'elevation', 22)));
        end
        asserts.isequal(tracePoints(f), tracePoints(g));
        for pair = [fh gh; ah bh]
          __web_figure_action__(pair(1), jsonencode(struct('action', 'restore', 'axesHandle', pair(2))));
        end
        asserts.isapprox([a.XLim; a.YLim; a.ZLim], original, 1e-8);
        asserts.isapprox(a.CameraPosition, camera, 1e-8);
        asserts.isequal(tracePoints(f), tracePoints(g));
        orientation = a.View;
        for pair = [fh gh; ah bh]
          __web_figure_action__(pair(1), jsonencode(struct('action', 'fit', 'axesHandle', pair(2))));
        end
        asserts.isequal(a.View, orientation);
        fitted = [a.XLim; a.YLim; a.ZLim];
        for k = 1:3
          if bitget(scales, k), fitted(k, :) = log10(fitted(k, :)); end
        end
        asserts.isapprox(fitted, [b.XLim; b.YLim; b.ZLim], 1e-8);
        % Return to the same limits before testing another camera combination.
        for k = 1:3
          set(a, [char('X' + k - 1) 'Lim'], original(k, :));
          set(b, [char('X' + k - 1) 'Lim'], [0 2]);
        end
      end
    end
  end
end
% Reject overflowing logarithmic pans without partly changing graphics objects.
before = [a.CameraPosition a.CameraTarget a.XLim a.YLim a.ZLim];
caught = false;
try
  __web_figure_action__(fh, jsonencode(struct('action', 'pan', ...
    'axesHandle', ah, 'dxFrac', 1e6, 'dyFrac', 1e6)));
catch
  caught = true;
end
asserts.istrue(caught);
asserts.isequal([a.CameraPosition a.CameraTarget a.XLim a.YLim a.ZLim], before);
close(f); close(g);
