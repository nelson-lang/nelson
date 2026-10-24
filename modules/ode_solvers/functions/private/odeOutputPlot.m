%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function status = odeOutputPlot(kind, t, y, flag)
  persistent states
  status = 0;
  if isempty(states)
    states = struct();
  end
  if isstring(flag)
    flag = char(flag);
  end
  state = odeOutputPlotGetState(states, kind);
  switch flag
    case 'init'
      state = odeOutputPlotInitialState(kind, t, y);
      states.(kind) = state;
    case 'done'
      odeOutputPlotFlush(kind, state);
      states.(kind) = state;
      drawnow();
    case ''
      if isempty(t) || isempty(y)
        return
      end
      state = odeOutputPlotAppend(kind, state, t, y);
      states.(kind) = state;
    otherwise
      error('Nelson:ode_solvers:unknownOutputCallbackFlag', 'Unknown output callback flag.');
  end
end
%=============================================================================
function state = odeOutputPlotEmptyState()
  state = struct('XData', [], 'YData', [], 'ZData', [], 'Handles', [], 'DrawCount', 0);
end
%=============================================================================
function state = odeOutputPlotGetState(states, kind)
  if isfield(states, kind)
    state = states.(kind);
  else
    state = odeOutputPlotEmptyState();
  end
end
%=============================================================================
function state = odeOutputPlotInitialState(kind, t, y)
  state = odeOutputPlotGetReusableState(kind);
  existingXData = state.XData;
  state = odeOutputPlotAppend(kind, state, t(1), y);
  if strcmp(kind, 'time') && length(t) >= 2
    bounds = [min([existingXData(:); t(:)]) max([existingXData(:); t(:)])];
    if bounds(1) ~= bounds(2)
      xlim(bounds);
    end
  end
end
%=============================================================================
function state = odeOutputPlotGetReusableState(kind)
  state = odeOutputPlotEmptyState();
  ax = gca();
  if ishold() && strcmp(kind, 'time')
    children = ax.Children;
    for k = 1:numel(children)
      child = children(k);
      if strcmp(child.Type, 'line') && strcmp(child.Tag, 'odeOutputPlot')
        state.XData = child.XData;
        state.YData = child.YData;
        state.Handles = {child};
        state.DrawCount = 0;
        return
      end
    end
  end
end
%=============================================================================
function state = odeOutputPlotAppend(kind, state, t, y)
  [newX, newY, newZ] = odeOutputPlotData(kind, t, y);
  state.XData = [state.XData newX];
  state.YData = [state.YData newY];
  state.ZData = [state.ZData newZ];
  if isempty(state.Handles) || ~odeOutputPlotHandlesValid(state.Handles)
    state.Handles = odeOutputPlotCreate(kind, state);
    state.DrawCount = 0;
  else
    state.DrawCount = state.DrawCount + 1;
    if odeOutputPlotShouldDraw(state)
      odeOutputPlotUpdate(kind, state);
      state.DrawCount = 0;
    end
  end
end
%=============================================================================
function tf = odeOutputPlotShouldDraw(state)
  tf = length(state.XData) <= 16 || state.DrawCount >= 250;
end
%=============================================================================
function odeOutputPlotFlush(kind, state)
  if isempty(state.Handles) || ~odeOutputPlotHandlesValid(state.Handles)
    return
  end
  odeOutputPlotUpdate(kind, state);
end
%=============================================================================
function [x, y, z] = odeOutputPlotData(kind, t, y)
  z = [];
  t = t(:).';
  y = odeOutputPlotNormalizeSolution(t, y);
  switch kind
    case 'time'
      x = t;
    case 'phase2'
      odeOutputPlotCheckComponents(y, 2);
      x = y(1, :);
      y = y(2, :);
    case 'phase3'
      odeOutputPlotCheckComponents(y, 3);
      x = y(1, :);
      z = y(3, :);
      y = y(2, :);
    otherwise
      error('Nelson:ode_solvers:unknownOutputPlotType', 'Unknown output plot type.');
  end
end
%=============================================================================
function y = odeOutputPlotNormalizeSolution(t, y)
  if isvector(y)
    y = y(:);
  end
  if size(y, 2) ~= length(t)
    if size(y, 1) == length(t)
      y = y.';
    elseif length(t) ~= 1
      error('Nelson:ode_solvers:incompatibleTimeSolutionSizes', 'Incompatible time and solution sizes.');
    end
  end
end
%=============================================================================
function odeOutputPlotCheckComponents(y, n)
  if size(y, 1) < n
    error('Nelson:ode_solvers:notEnoughSolutionComponents', 'Not enough solution components.');
  end
end
%=============================================================================
function handles = odeOutputPlotCreate(kind, state)
  ax = gca();
  switch kind
    case 'time'
      handles = odeOutputPlotCreateTimeLines(ax, state.XData, state.YData);
      xlabel(_('Time'));
      ylabel(_('Solution'));
    case 'phase2'
      handles = line('XData', state.XData, 'YData', state.YData, 'Parent', ax);
      handles.Tag = 'odeOutputPlot';
      xlabel('y(1)');
      ylabel('y(2)');
    case 'phase3'
      handles = line('XData', state.XData, 'YData', state.YData, ...
        'ZData', state.ZData, 'Parent', ax);
      handles.Tag = 'odeOutputPlot';
      xlabel('y(1)');
      ylabel('y(2)');
      zlabel('y(3)');
      view(3);
  end
end
%=============================================================================
function handles = odeOutputPlotCreateTimeLines(ax, x, y)
  handles = cell(size(y, 1), 1);
  wasHold = ishold();
  for k = 1:size(y, 1)
    handles{k} = line('XData', x, 'YData', y(k, :), 'Parent', ax);
    handles{k}.Tag = 'odeOutputPlot';
    hold on
  end
  if ~wasHold
    hold off
  end
end
%=============================================================================
function tf = odeOutputPlotHandlesValid(handles)
  if iscell(handles)
    tf = ~isempty(handles);
    for k = 1:length(handles)
      tf = tf && ishandle(handles{k});
    end
  else
    tf = ~isempty(handles) && ishandle(handles);
  end
end
%=============================================================================
function odeOutputPlotUpdate(kind, state)
  switch kind
    case 'time'
      for k = 1:length(state.Handles)
        state.Handles{k}.XData = state.XData;
        state.Handles{k}.YData = state.YData(k, :);
      end
    case 'phase2'
      state.Handles.XData = state.XData;
      state.Handles.YData = state.YData;
    case 'phase3'
      state.Handles.XData = state.XData;
      state.Handles.YData = state.YData;
      state.Handles.ZData = state.ZData;
  end
end
%=============================================================================
