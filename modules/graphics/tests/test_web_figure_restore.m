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
a1 = subplot(2, 2, 1); plot(1:5);
a2 = subplot(2, 2, 2); plot3([0 1 2], [0 1 0], [0 1 2]);
a3 = subplot(2, 2, 3); surf(peaks(12));
a4 = subplot(2, 2, 4); plot(5:-1:1);
view(a2, -52, 20);
set(a3, 'Projection', 'perspective', 'XLim', [-1 15], 'YLim', [-2 16], ...
  'ZLim', [-10 12], 'CameraPosition', [40 -60 30], 'CameraTarget', [4 5 1], ...
  'CameraUpVector', [0 0 1], 'CameraViewAngle', 18);
drawnow();
fh = __web_graphics_handle__(f, 'number');
slots = [a1.Position; a2.Position; a3.Position; a4.Position];
untouched = [a1.XLim a1.YLim; a4.XLim a4.YLim];
props = {'View', 'Projection', 'XLim', 'YLim', 'ZLim', 'CameraPosition', ...
  'CameraTarget', 'CameraUpVector', 'CameraViewAngle', 'XLimMode', 'YLimMode', ...
  'ZLimMode', 'CameraPositionMode', 'CameraTargetMode', 'CameraUpVectorMode', ...
  'CameraViewAngleMode', 'DataAspectRatioMode', 'PlotBoxAspectRatioMode'};
references = {};
for ax = [a2 a3]
  reference = {};
  for k = 1:numel(props)
    reference{k} = get(ax, props{k});
  end
  references{end + 1} = reference;
  ah = __web_graphics_handle__(ax, 'number');
  % Reading inspector state must neither capture nor change the view.
  __web_figure_action__(fh, jsonencode(struct('action', 'state', 'axesHandle', ah)));
  __web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, 'dxFrac', 0.15, 'dyFrac', -0.1)));
  __web_figure_action__(fh, jsonencode(struct('action', 'zoom', 'axesHandle', ah, 'factor', 2)));
  __web_figure_action__(fh, jsonencode(struct('action', 'view', 'axesHandle', ah, 'azimuth', 45, 'elevation', 35, 'resetCamera', true)));
end
for index = 1:2
  ax = [a2 a3]; ax = ax(index);
  ah = __web_graphics_handle__(ax, 'number');
  __web_figure_action__(fh, jsonencode(struct('action', 'restore', 'axesHandle', ah)));
  for k = 1:numel(props)
    actual = get(ax, props{k}); expected = references{index}{k};
    if isnumeric(expected)
      asserts.isapprox(actual, expected, 1e-8);
    else
      asserts.isequal(actual, expected);
    end
  end
end
asserts.isequal([a1.Position; a2.Position; a3.Position; a4.Position], slots);
asserts.isequal([a1.XLim a1.YLim; a4.XLim a4.YLim], untouched);
% Each subplot can explicitly replace its reference, without changing data/style.
ah = __web_graphics_handle__(a1, 'number');
__web_figure_action__(fh, jsonencode(struct('action', 'properties', 'axesHandle', ah, ...
  'properties', struct('XLim', [10 20], 'YLim', [-10 10]))));
__web_figure_action__(fh, jsonencode(struct('action', 'reference', 'axesHandle', ah)));
__web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, 'dxFrac', 0.2, 'dyFrac', 0.1)));
a1.XGrid = 'on';
__web_figure_action__(fh, jsonencode(struct('action', 'restore', 'axesHandle', ah)));
asserts.isequal(a1.XLim, [10 20]);
asserts.isequal(a1.YLim, [-10 10]);
asserts.isequal(char(a1.XGrid), 'on');
% Resizing or remounting a frontend does not replace the engine-owned reference.
__web_figure_size__(fh, 650, 400);
__web_figure_action__(fh, jsonencode(struct('action', 'fit', 'axesHandle', ah)));
__web_figure_action__(fh, jsonencode(struct('action', 'restore', 'axesHandle', ah)));
asserts.isequal(a1.XLim, [10 20]);
close(f);

% A rotation through the compact renderer also captures the original view.
f = figure('Visible', 'off'); ax = axes('Parent', f);
plot3(ax, [0 1 2], [0 1 0], [0 1 2]); view(ax, -52, 20); drawnow();
fh = __web_graphics_handle__(f, 'number'); ah = __web_graphics_handle__(ax, 'number');
__web_figure_action__(fh, jsonencode(struct('action', 'state')));
__web_figure_action__(fh, jsonencode(struct('action', 'properties', 'axesHandle', ah, ...
  'properties', struct('XGrid', 'on'))));
view(ax, -48, 22); drawnow();
__web_display_list__(f, ax, 45, 35, true);
__web_figure_action__(fh, jsonencode(struct('action', 'restore', 'axesHandle', ah)));
asserts.isequal(ax.View, [-48 22]);
close(f);
% Deleted axes may have their numeric handles reused; references must not leak.
f = figure('Visible', 'off'); ax = axes('Parent', f);
plot3(ax, [0 1 2], [0 1 0], [0 1 2]); view(ax, 60, 15); drawnow();
fh = __web_graphics_handle__(f, 'number'); ah = __web_graphics_handle__(ax, 'number');
__web_display_list__(f, ax, 45, 35, false);
__web_figure_action__(fh, jsonencode(struct('action', 'restore', 'axesHandle', ah)));
asserts.isequal(ax.View, [60 15]);
close(f);
