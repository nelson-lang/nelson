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
%=============================================================================
f = figure('Visible', 'off', 'Name', 'Inspector');
ax = axes('Parent', f);
h = plot3(ax, [0 1 2], [0 1 0], [0 1 2]);
view(ax, -37.5, 30);
drawnow();
fh = __web_graphics_handle__(f, 'number');
ah = __web_graphics_handle__(ax, 'number');
lh = __web_graphics_handle__(h, 'number');
asserts.isequal(__web_graphics_handle__(fh, 'object'), f);
state = __web_figure_state__(fh);
asserts.isequal(state.name, 'Inspector');
asserts.isequal(state.axes{1}.handle, ah);
asserts.isequal(state.axes{1}.View, ax.View);
asserts.isequal(state.numberTitle, 'on');
asserts.isequal(state.axes{1}.XGrid, 'off');
asserts.istrue(contains(jsonencode(state), 'Inspector'));
asserts.istrue(contains(jsonencode(__web_figure_list__()), 'Inspector'));
set(f, 'Name', 'Hélice', 'NumberTitle', 'off');
metadata = __web_figure_list__();
asserts.isequal(metadata{1}.title, 'Hélice');

request = struct('action', 'properties', 'axesHandle', ah, ...
  'properties', struct('XGrid', 'on', 'XLim', [-1 4], 'XLimMode', 'manual'));
reply = jsondecode(__web_figure_action__(fh, jsonencode(request)));
asserts.isequal(char(ax.XGrid), 'on');
asserts.isequal(ax.XLim, [-1 4]);
asserts.isequal(reply.state.axes.XLim(:)', ax.XLim);
request = struct('action', 'properties', 'axesHandle', ah, 'objectHandle', lh, ...
  'properties', struct('LineWidth', 3, 'Color', [1 0 0]));
__web_figure_action__(fh, jsonencode(request));
asserts.isequal(h.LineWidth, 3);
asserts.isequal(h.Color, [1 0 0]);
request = struct('action', 'properties', 'axesHandle', ah, 'target', 'title', ...
  'properties', struct('String', 'Hélice — titre édité'));
__web_figure_action__(fh, jsonencode(request));
asserts.isequal(ax.Title.String, 'Hélice — titre édité');

campos(ax, [0 -30 0]);
request = struct('action', 'view', 'axesHandle', ah, 'azimuth', 45, ...
  'elevation', 35, 'resetCamera', true);
__web_figure_action__(fh, jsonencode(request));
asserts.isequal(ax.View, [45 35]);
asserts.isequal(ax.CameraPositionMode, 'auto');
camva(ax, 20);
__web_figure_action__(fh, jsonencode(struct('action', 'zoom', 'axesHandle', ah, 'factor', 2)));
% A factor-2 zoom scales the camera view angle (camzoom semantics); for this
% 3-D view that is not an exact halving of 20 degrees.
asserts.isapprox(camva(ax), 10.076737546594986, 1e-6);
p0 = campos(ax); t0 = camtarget(ax);
__web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, ...
  'dxFrac', 0.1, 'dyFrac', 0.2)));
asserts.isapprox(campos(ax) - p0, camtarget(ax) - t0, 1e-8);

__web_figure_action__(fh, jsonencode(struct('action', 'floor', 'axesHandle', ah, 'enabled', true)));
projection = findall(ax, 'Tag', '__web_floor_projection__');
asserts.isequal(numel(projection), 1);
h.XData = [1 3 4];
h.Visible = 'off';
ax.ZLim = [-5 5];
drawnow();
asserts.isequal(projection.XData, h.XData);
asserts.isequal(projection.ZData, [-5 -5 -5]);
asserts.isequal(char(projection.Visible), 'off');
h.Visible = 'on';
__web_figure_action__(fh, jsonencode(struct('action', 'floor', 'axesHandle', ah, 'enabled', false)));
asserts.istrue(isempty(findall(ax, 'Tag', '__web_floor_projection__')));
__web_figure_action__(fh, jsonencode(struct('action', 'fit', 'axesHandle', ah)));
asserts.isequal(ax.XLimMode, 'auto');
asserts.isequal(ax.CameraViewAngleMode, 'auto');

other = figure('Visible', 'off');
otherAxes = axes('Parent', other);
request = struct('action', 'properties', ...
  'axesHandle', __web_graphics_handle__(otherAxes, 'number'), ...
  'properties', struct('XGrid', 'on'));
caught = false;
try
  __web_figure_action__(fh, jsonencode(request));
catch
  caught = true;
end
asserts.istrue(caught);
asserts.isequal(char(otherAxes.XGrid), 'off');
close(other);

view(ax, 2);
set(ax, 'XScale', 'log', 'XLim', [1 100], 'YLim', [0 10]);
__web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, ...
  'dxFrac', 0.5, 'dyFrac', 0.1)));
asserts.isapprox(ax.XLim, [0.1 10], 1e-8);
asserts.isapprox(ax.YLim, [1 11], 1e-8);
set(ax, 'XScale', 'linear', 'XLim', [0 10], 'YLim', [-5 5]);
__web_figure_action__(fh, jsonencode(struct('action', 'zoom', 'axesHandle', ah, 'factor', 2)));
asserts.isapprox(ax.XLim, [2.5 7.5], 1e-8);
asserts.isapprox(ax.YLim, [-2.5 2.5], 1e-8);
close(f);

% A pick returns the actual sample in data coordinates, without running callbacks.
f = figure('Visible', 'off', 'Position', [100 100 400 300]);
ax = axes('Parent', f, 'Position', [0.1 40 / 300 0.8 200 / 300]);
set(ax, 'XLim', [0 10], 'YLim', [0 100]);
h = line('Parent', ax, 'XData', [0 5 10], 'YData', [0 50 100]);
drawnow();
__web_display_list__(f);
point = jsondecode(__web_figure_pick__(f, 200, 140));
asserts.istrue(point.found);
asserts.isequal(point.index, 2);
asserts.isequal(point.position(:)', [5 50 0]);
asserts.isequal(point.objectHandle, __web_graphics_handle__(h, 'number'));
h.ZData = [0 1 2];
view(ax, -37.5, 30);
set(ax, 'ZLim', [0 2]);
drawnow();
frame = jsondecode(__web_display_list__(f));
asserts.istrue(frame.is3d);
view(ax, 2);
frame = jsondecode(__web_display_list__(f));
asserts.istrue(frame.is3d);
view(ax, -37.5, 30);
__web_display_list__(f);
point = jsondecode(__web_figure_pick__(f, 200, 140));
asserts.istrue(point.found);
asserts.isequal(point.index, 2);
asserts.isequal(point.position(:)', [5 50 1]);
close(f);

% Each subplot retains its own view, limits, and hit-test rectangle.
f = figure('Visible', 'off');
a1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.8]);
plot(a1, 1:5);
a2 = axes('Parent', f, 'Position', [0.6 0.1 0.3 0.8]);
plot(a2, 1:5);
fh = __web_graphics_handle__(f, 'number');
ah = __web_graphics_handle__(a1, 'number');
initial = a2.XLim;
__web_figure_action__(fh, jsonencode(struct('action', 'pan', 'axesHandle', ah, ...
  'dxFrac', 0.2, 'dyFrac', 0)));
asserts.isequal(a2.XLim, initial);
asserts.istrue(any(a1.XLim ~= initial));
frame = jsondecode(__web_display_list__(f));
asserts.isequal(numel(frame.rotatableAxes), 2);
asserts.istrue(~frame.is3d);
close(f);

% Surface and scatter color modes also remain JSON-serializable.
f = figure('Visible', 'off');
surf(peaks(5));
fh = __web_graphics_handle__(f, 'number');
state = jsondecode(__web_figure_action__(fh, jsonencode(struct('action', 'state'))));
asserts.isequal(state.state.axes.objects.type, 'surface');
close(f);
f = figure('Visible', 'off');
scatter3([0 1 2], [0 1 0], [0 1 2]);
fh = __web_graphics_handle__(f, 'number');
state = jsondecode(__web_figure_action__(fh, jsonencode(struct('action', 'state'))));
asserts.isequal(state.state.axes.objects.type, 'scatter');
close(f);

% The existing compact View path also targets axes inside a panel.
f = figure('Visible', 'off');
p = uipanel('Parent', f);
ax = axes('Parent', p);
plot3(ax, [0 1 2], [0 1 0], [0 1 2]);
frame = jsondecode(__web_display_list__(f, ax, 45, 35, false));
asserts.isequal(ax.View, [45 35]);
asserts.isequal(frame.rotatableAxes.handle, __web_graphics_handle__(ax, 'number'));
close(f);
