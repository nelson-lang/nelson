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
f = figure('Visible', 'off');
layout = tiledlayout(2, 2);
for index = 1:4
  ax = nexttile(layout);
  surface = surf(ax, peaks(15));
  surface.DisplayName = sprintf('Surface %d', index);
  colorbar(ax);
end
drawnow();
handle = __web_graphics_handle__(f, 'number');
current = f.CurrentAxes;
profile('clear'); profile('on');
state = __web_figure_state__(handle);
profile('off');
stats = profile('info'); profile('clear');
asserts.isequal(sum(strcmp({stats.FunctionName}, '__web_figure_state__')), 1);
asserts.isfalse(any(strcmp({stats.FunctionName}, 'graphics_object_get')));
asserts.isfalse(any(strcmp({stats.FunctionName}, 'isprop')));
asserts.isequal(numel(state.axes), 4);
asserts.isequal(f.CurrentAxes, current);
for index = 1:4
  entry = state.axes{index};
  ax = __web_graphics_handle__(entry.handle, 'object');
  asserts.isequal(entry.View, ax.View);
  asserts.isequal(entry.XLim, ax.XLim);
  asserts.isequal(entry.CameraPositionMode, ax.CameraPositionMode);
  asserts.isequal(entry.XGrid, char(ax.XGrid));
  asserts.isequal(numel(entry.objects), 1);
  object = entry.objects{1};
  asserts.isequal(object.visible, 'on');
  asserts.isequal(object.type, 'surface');
  asserts.isequal(object.LineStyle, '-');
end
asserts.isequal(__web_figure_snapshot__(f).handle, handle);
empty = figure('Visible', 'off');
emptyState = __web_figure_state__(__web_graphics_handle__(empty, 'number'));
asserts.istrue(isempty(emptyState.axes));
asserts.istrue(isempty(empty.CurrentAxes));
__web_figure_state__(handle);
asserts.isequal(gcf(), empty);
for invalid = [NaN Inf -1 0.5]
  failed = false;
  try
    __web_figure_state__(invalid);
  catch
    failed = true;
  end
  asserts.istrue(failed);
end
close(empty);
__web_figure_history_data__(f, struct('undo', {{1}}, 'redo', {{}}));
asserts.istrue(__web_figure_state__(handle).canUndo);
close(f);
fresh = figure('Visible', 'off');
asserts.istrue(isempty(__web_figure_history_data__(fresh)));
asserts.isfalse(__web_figure_state__(__web_graphics_handle__(fresh, 'number')).canUndo);
close(fresh);
