%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function web_figure_isolate(f, ax, request)
  target = __web_graphics_handle__(request.objectHandle, 'object');
  if ~isgraphics(target) || ~any(strcmp(get(target, 'Type'), {'line', 'scatter', 'surface', 'patch'})) ...
      || ~isequal(ancestor(target, 'axes'), ax) ...
      || ~isequal(ancestor(target, 'figure'), f)
    error('Nelson:graphics:invalidTarget', 'Select a plotted object of these axes.');
  end
  key = sprintf('axes_%.0f', __web_graphics_handle__(ax, 'number'));
  data = __web_figure_history_data__(f);
  if isempty(data)
    data = struct('undo', {{}}, 'redo', {{}});
  end
  saved = [];
  if isfield(data, 'isolation') && isfield(data.isolation, key)
    saved = data.isolation.(key);
  end
  state = __web_figure_state__(__web_graphics_handle__(f, 'number'));
  objects = {};
  for axis = state.axes
    if axis{1}.handle == __web_graphics_handle__(ax, 'number')
      objects = axis{1}.objects;
    end
  end
  if ~isempty(saved) && saved.target == request.objectHandle
    for item = saved.objects
      object = __web_graphics_handle__(item{1}.handle, 'object');
      if isgraphics(object) && isequal(ancestor(object, 'axes'), ax)
        set(object, 'Visible', item{1}.visible);
      end
    end
    data.isolation = rmfield(data.isolation, key);
    __web_figure_history_data__(f, data);
    return;
  end
  if isempty(saved)
    saved = struct('target', request.objectHandle, 'objects', {objects});
  end
  saved.target = request.objectHandle;
  data.isolation.(key) = saved;
  __web_figure_history_data__(f, data);
  for item = objects
    object = __web_graphics_handle__(item{1}.handle, 'object');
    visible = 'off';
    if item{1}.handle == request.objectHandle
      visible = 'on';
    end
    set(object, 'Visible', visible);
  end
end
