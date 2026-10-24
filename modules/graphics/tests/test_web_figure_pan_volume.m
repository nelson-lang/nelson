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
a2 = subplot(2, 2, 2); plot3([0 1 2], [0 1 0], [0 1 2]);
a3 = subplot(2, 2, 3); surf(peaks(15));
a4 = subplot(2, 2, 4); plot(5:-1:1);
drawnow();
fh = __web_graphics_handle__(f, 'number');
slots = [a1.Position; a2.Position; a3.Position; a4.Position];
others = [a1.XLim a1.YLim; a4.XLim a4.YLim];
for ax = [a2 a3]
  ah = __web_graphics_handle__(ax, 'number');
  for projection = {'orthographic', 'perspective'}
    set(ax, 'Projection', projection{1});
    __web_figure_action__(fh, jsonencode(struct('action', 'fit', 'axesHandle', ah)));
    for movement = [0.1 -0.6; -0.05 0.45]
      limits = [ax.XLim; ax.YLim; ax.ZLim];
      camera = ax.CameraPosition; target = ax.CameraTarget;
      orientation = ax.View;
      __web_figure_action__(fh, jsonencode(struct('action', 'pan', ...
        'axesHandle', ah, 'dxFrac', movement(1), 'dyFrac', movement(2))));
      delta = ax.CameraTarget - target;
      % Translating only the camera makes the whole axes box leave its subplot.
      asserts.istrue(norm(delta) > 1e-6);
      asserts.isapprox(ax.CameraPosition - camera, delta, 1e-8);
      asserts.isapprox([ax.XLim; ax.YLim; ax.ZLim], limits + delta(:) * [1 1], 1e-8);
      asserts.isequal({ax.XLimMode, ax.YLimMode, ax.ZLimMode}, {'manual', 'manual', 'manual'});
      asserts.isequal(ax.View, orientation);
      asserts.isequal([a1.Position; a2.Position; a3.Position; a4.Position], slots);
      asserts.isequal([a1.XLim a1.YLim; a4.XLim a4.YLim], others);
    end
  end
end
close(f);
