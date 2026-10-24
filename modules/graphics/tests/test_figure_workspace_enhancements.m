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
f = figure('Visible', 'off', 'Position', [0 0 400 300]);
ax = axes('Parent', f);
first = line('Parent', ax, 'XData', [0 1 2], 'YData', [0 1 2], 'DisplayName', 'Signal');
second = line('Parent', ax, 'XData', [0 1 2], 'YData', [2 1 0]);
third = line('Parent', ax, 'XData', [0 1 2], 'YData', [2 2 2], 'Visible', 'off');
set(ax, 'XLim', [0 2], 'YLim', [0 2]);
handle = __web_graphics_handle__(f, 'number');
object = __web_graphics_handle__(first, 'number');
axisHandle = __web_graphics_handle__(ax, 'number');
state = __web_figure_state__(handle);
asserts.isfalse(state.canUndo);
request = struct('action', 'properties', 'objectHandle', object, ...
  'properties', struct('Color', [1 0 0], 'LineStyle', '--', 'Marker', 'o', 'MarkerSize', 9));
reply = execute(handle, request);
asserts.istrue(reply.state.canUndo);
asserts.isequal(first.Color, [1 0 0]);
asserts.isequal(char(first.LineStyle), '--');
failed = false;
try
  execute(handle, struct('action', 'properties', 'objectHandle', object, ...
    'properties', struct('Color', [0 0 1], 'Marker', 'invalid-marker')));
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isequal(first.Color, [1 0 0]);
asserts.isequal(char(first.Marker), 'o');
execute(handle, struct('action', 'undo'));
asserts.isequal(char(first.LineStyle), '-');
asserts.isequal(char(first.Marker), 'none');
execute(handle, struct('action', 'redo'));
asserts.isequal(first.MarkerSize, 9);
before = ax.View;
for angle = [10 20 30]
  execute(handle, struct('action', 'view', 'azimuth', angle, 'elevation', 40, 'historyGroup', 'drag-one'));
end
execute(handle, struct('action', 'undo'));
asserts.isapprox(ax.View, before, 1e-10);
execute(handle, struct('action', 'redo'));
asserts.isapprox(ax.View, [30 40], 1e-10);
request = struct('action', 'isolate', 'axesHandle', axisHandle, 'objectHandle', object);
execute(handle, request);
asserts.isequal(char(first.Visible), 'on');
asserts.isequal(char(second.Visible), 'off');
execute(handle, request);
asserts.isequal(char(second.Visible), 'on');
asserts.isequal(char(third.Visible), 'off');
execute(handle, struct('action', 'undo'));
asserts.isequal(char(second.Visible), 'off');
execute(handle, struct('action', 'redo'));
asserts.isequal(char(second.Visible), 'on');
view(ax, 0, 90); drawnow();
point = execute(handle, struct('action', 'step', 'objectHandle', object, 'index', 2));
asserts.istrue(point.point.found);
asserts.isequal(point.point.index, 2);
asserts.isequal(point.point.count, 3);
asserts.isequal(point.point.position(:)', [1 1 0]);
point = execute(handle, struct('action', 'step', 'objectHandle', object, 'index', -50));
asserts.isequal(point.point.index, 1);
point = execute(handle, struct('action', 'step', 'objectHandle', object, 'index', 50));
asserts.isequal(point.point.index, 3);
execute(handle, struct('action', 'properties', 'objectHandle', object, 'properties', struct('LineWidth', 2)));
execute(handle, struct('action', 'undo'));
execute(handle, struct('action', 'properties', 'objectHandle', object, 'properties', struct('LineWidth', 3)));
asserts.isfalse(__web_figure_state__(handle).canRedo);
other = figure('Visible', 'off');
otherAxes = axes('Parent', other);
foreign = line('Parent', otherAxes, 'XData', 1:3, 'YData', 1:3);
failed = false;
try
  execute(handle, struct('action', 'step', 'objectHandle', __web_graphics_handle__(foreign, 'number'), 'index', 1));
catch
  failed = true;
end
asserts.istrue(failed);
close(other);
delete(second);
failed = false;
try
  execute(handle, struct('action', 'undo'));
catch
  failed = true;
end
asserts.istrue(failed);
asserts.isfalse(__web_figure_state__(handle).canUndo);
close(f);

function result = execute(handle, request)
  result = jsondecode(__web_figure_action__(handle, jsonencode(request), false));
end
