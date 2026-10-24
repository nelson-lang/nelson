%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function response = __web_figure_action__(handle, encoded, renderDisplayList)
  if nargin < 3
    renderDisplayList = true;
  end
  f = __web_graphics_handle__(handle, 'object');
  if ~isgraphics(f, 'figure')
    error('Nelson:graphics:expectedFigure', 'Figure object expected.');
  end
  request = jsondecode(encoded);
  action = request.action;
  inspectHandle = -1;
  if isfield(request, 'inspectHandle')
    inspectHandle = request.inspectHandle;
  end
  % A WebGL-capable client sets softwareFallback=false so the figure renders
  % without the costly software raster it would otherwise discard.
  softwareFallback = ~isfield(request, 'softwareFallback') || request.softwareFallback;
  if strcmp(action, 'export')
    response = jsonencode(web_figure_export(f, request));
    return;
  end
  ax = get(f, 'CurrentAxes');
  if isfield(request, 'axesHandle') && request.axesHandle > 0
    ax = __web_graphics_handle__(request.axesHandle, 'object');
  end
  % A figure containing subplots may expose CurrentAxes as a handle array
  % while a pointer action is being dispatched.  All navigation helpers are
  % scalar-axis operations; passing that array into get/set or is2D raises
  % conversionNotSupported.  The client supplies axesHandle for pointer
  % gestures, but keep the fallback safe for wheel/restore actions arriving
  % during a subplot refresh as well.
  if ~isempty(ax) && numel(ax) > 1
    ax = ax(1);
  end
  if isfield(request, 'axesHandle') && request.axesHandle > 0 && ...
      (~isgraphics(ax, 'axes') || ~isequal(ancestor(ax, 'figure'), f))
    error('Nelson:graphics:invalidAxes', 'Axes belongs to another figure.');
  end
  compactView = isfield(request, 'compactView') && request.compactView;
  % A compact frame is only valid when the axes decorations do not need to be
  % repainted.  Visible axes can otherwise enter a transient handle-array
  % state during drawnow/view updates, independently of the raster fallback
  % selected by the client.  Keep the compact path for hidden axes and use the
  % regular display list for visible axes in every frontend.
  if compactView && ~isempty(ax)
    if ~strcmp(char(get(ax, 'Visible')), 'off')
      compactView = false;
    end
  end
  if strcmp(action, 'state')
    response = jsonencode(struct('state', __web_figure_state_full__(handle, inspectHandle), ...
      'figureList', {__web_figure_list__()}));
    return;
  end
  if strcmp(action, 'inspect')
    objectHandle = 0;
    if isfield(request, 'objectHandle')
      objectHandle = request.objectHandle;
    end
    response = jsonencode(__web_figure_inspect__(handle, objectHandle));
    return;
  end
  if isempty(ax) && ~any(strcmp(action, {'properties', 'undo', 'redo'}))
    error('Nelson:graphics:expectedAxes', 'Axes object expected.');
  end
  if any(strcmp(action, {'view', 'zoom', 'pan', 'fit'}))
    web_figure_reference_view(ax, 'capture');
  end
  record = any(strcmp(action, {'properties', 'view', 'zoom', 'pan', 'fit', 'restore', 'floor', 'isolate'}));
  if record
    before = __web_figure_history__(f, 'begin');
  end
  try
    switch action
      case {'undo', 'redo'}
        __web_figure_history__(f, action);
      case 'isolate'
        web_figure_isolate(f, ax, request);
      case 'select'
        set(f, 'CurrentAxes', ax);
      case {'restore', 'reference'}
        web_figure_reference_view(ax, action);
      case 'properties'
        web_figure_properties(f, ax, request);
      case 'view'
        if isfield(request, 'resetCamera') && request.resetCamera
          set(ax, 'CameraPositionMode', 'auto', 'CameraTargetMode', 'auto', ...
            'CameraUpVectorMode', 'auto', 'CameraViewAngleMode', 'auto');
        end
        if renderDisplayList
          % Absolute azimuth/elevation must orbit a manual camera about its
          % target at constant radius, like the interactive drag path. A bare
          % view() only updates View, which a manual camera ignores for its
          % position; routing through the shared setter keeps both in sync (and
          % is equivalent to view() for an automatic camera). The full figure
          % frame is rebuilt below; the compact frame here is discarded.
          __web_display_list__(f, ax, request.azimuth, request.elevation, true, softwareFallback);
        else
          % The caller only needs the updated camera state here. Avoid
          % rendering a frame that will be discarded by the compact path.
          view(ax, request.azimuth, request.elevation);
        end
      case 'zoom'
        if is2D(ax)
          web_figure_navigate_2d(ax, request);
        else
          camzoom(ax, request.factor);
        end
      case 'pan'
        if is2D(ax)
          web_figure_navigate_2d(ax, request);
        else
          web_figure_pan_3d(ax, request);
        end
      case 'fit'
        set(ax, 'XLimMode', 'auto', 'YLimMode', 'auto', 'ZLimMode', 'auto', ...
          'CameraViewAngleMode', 'auto', 'CameraPositionMode', 'auto', ...
          'CameraTargetMode', 'auto', 'CameraUpVectorMode', 'auto');
      case 'floor'
        web_figure_floor(ax, request.enabled);
      case 'pick'
        point = jsondecode(__web_figure_pick__(f, request.x, request.y));
        response = jsonencode(struct('state', __web_figure_state_full__(handle), 'point', point));
        return;
      case 'step'
        object = __web_graphics_handle__(request.objectHandle, 'object');
        if ~isgraphics(object) || ~isequal(ancestor(object, 'figure'), f)
          error('Nelson:graphics:invalidTarget', 'Object belongs to another figure.');
        end
        point = jsondecode(__web_figure_pick__(f, 0, 0, object, request.index));
        response = jsonencode(struct('state', __web_figure_state_full__(handle), 'point', point));
        return;
      otherwise
        error('Nelson:graphics:invalidAction', 'Unknown figure action.');
    end
  catch failure
    if record
      web_figure_restore_snapshot(f, before, __web_figure_history__(f, 'begin'));
    end
    rethrow(failure);
  end
  if ~renderDisplayList
    drawnow();
  end
  if record
    group = '';
    if isfield(request, 'historyGroup')
      group = request.historyGroup;
    end
    __web_figure_history__(f, 'commit', before, group);
  end
  metadata = jsonencode(struct('state', __web_figure_state_full__(handle, inspectHandle), ...
    'figureList', {__web_figure_list__()}));
  if ~renderDisplayList
    response = metadata;
    % The compact flag is a rendering preference, not a reason to omit the
    % updated frame.  Visible axes are downgraded above to the regular display
    % list because compact mesh conversion is not safe while decorations are
    % being refreshed.  Always return that frame so zoom/pan/rotation can apply
    % the same update path as the WebView backend.
    if isfield(request, 'compactView') && ~isempty(ax)
      if compactView
        currentView = view(ax);
        displayList = __web_display_list__(f, ax, currentView(1), currentView(2), true, softwareFallback);
      else
        displayList = __web_display_list__(f, softwareFallback);
      end
      response = [metadata(1:end - 1), ',"displayList":', displayList, '}'];
    end
    return;
  end
  % Preserve the renderer's JSON arrays, including single-element draw lists.
  if strcmp(action, 'view') && isfield(request, 'compactView')
    if compactView
      displayList = __web_display_list__(f, ax, request.azimuth, request.elevation, true, softwareFallback);
    else
      displayList = __web_display_list__(f, softwareFallback);
    end
  else
    displayList = __web_display_list__(f, softwareFallback);
  end
  response = [metadata(1:end - 1), ',"displayList":', displayList, '}'];
end
