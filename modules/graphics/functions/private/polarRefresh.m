%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function polarRefresh(ax, state, manageDrawLater)
  if ~polarIsAxes(ax)
    return
  end
  if nargin < 2
    state = polarGetState(ax, false);
  end
  if nargin < 3
    manageDrawLater = true;
  end
  fig = [];
  previousDrawLater = [];
  if manageDrawLater
    [fig, previousDrawLater] = polarBeginDrawLater(ax);
  end
  try
    deleteValidHandles(state.GridHandles);
    state.GridHandles = gobjects(0, 1);
    state.DataHandles = validDataHandles(state.DataHandles, ax);
    state = updateAutoRadiusLimits(state);
    if strcmp(state.RTickMode, 'auto')
      state.RTick = polarNiceTicks(state.RLim);
    end
    if strcmp(state.ThetaTickMode, 'auto')
      state.ThetaTick = automaticThetaTicks(state.ThetaLim, state.ThetaAxisUnits);
    end
    if strcmp(state.RTickLabelMode, 'auto')
      state.RTickLabel = polarFormatTickLabels(state.RTick, false);
    end
    if strcmp(state.ThetaTickLabelMode, 'auto')
      state.ThetaTickLabel = polarThetaTickLabels(state.ThetaTick, state.ThetaLim, state.ThetaAxisUnits);
    end
    maxRadius = max(abs(state.RLim));
    if maxRadius <= 0 || ~isfinite(maxRadius)
      maxRadius = 1;
    end
    [xLimits, yLimits] = polarVisibleLimits(state.ThetaLim, maxRadius, ...
      state.ThetaAxisUnits, polarZeroOffset(state), polarDirSign(state));
    % 'axis off' on a polar axes hides the grid, labels and the background disc,
    % leaving only the plotted data. The disc is the axes Color, so drop it to
    % 'none' (the figure shows through); the grid/labels are created hidden below.
    gridVisible = polarGridVisibleValue(state);
    if strcmp(gridVisible, 'off')
      discColor = 'none';
    else
      discColor = [1 1 1];
    end
    set(ax, 'XLim', xLimits, 'YLim', yLimits, ...
      'PolarAxes', 'on', 'Visible', 'off', 'Box', 'off', 'Color', discColor, ...
      'XTick', [], 'YTick', [], 'DataAspectRatio', [1 1 1], ...
      'PlotBoxAspectRatio', [1 1 1]);
    [state.GridHandles, gridLines, gridLabels] = drawPolarGrid(ax, state, maxRadius, gridVisible);
    reorderPolarChildren(ax, state, gridLines, gridLabels);
    polarSetState(ax, state);
    if manageDrawLater
      polarEndDrawLater(fig, previousDrawLater);
    end
  catch exception
    if manageDrawLater
      polarEndDrawLater(fig, previousDrawLater);
    end
    error(exception.message);
  end
end
%=============================================================================
function deleteValidHandles(handles)
  if isempty(handles)
    return
  end
  try
    delete(handles);
  catch
    for k = 1:numel(handles)
      try
        delete(handles(k));
      catch
      end
    end
  end
end
%=============================================================================
function handles = validDataHandles(handles, ax)
  if isempty(handles)
    handles = gobjects(0, 1);
    return
  end
  valid = isgraphics(handles);
  handles = handles(valid);
  keep = false(size(handles));
  for k = 1:numel(handles)
    keep(k) = (isgraphics(handles(k), 'line') || isgraphics(handles(k), 'functionline') ...
      || isgraphics(handles(k), 'compassplot') || isgraphics(handles(k), 'bubblechart')) ...
      && isequal(handles(k).Parent, ax);
  end
  handles = handles(keep);
  handles = handles(:);
end
%=============================================================================
function state = updateAutoRadiusLimits(state)
  if ~strcmp(state.RLimMode, 'auto')
    return
  end
  maxRadius = 0;
  for k = 1:numel(state.DataHandles)
    h = state.DataHandles(k);
    if isgraphics(h, 'line') || isgraphics(h, 'functionline') || isgraphics(h, 'compassplot') ...
        || isgraphics(h, 'bubblechart')
      r = h.RData;
      if ~isempty(r)
        r = abs(r(isfinite(r)));
        if ~isempty(r)
          maxRadius = max(maxRadius, max(r(:)));
        end
      end
    end
  end
  state.RLim = polarNiceRLimit(maxRadius);
end
%=============================================================================
function vis = polarGridVisibleValue(state)
  vis = 'on';
  if isfield(state, 'GridVisible') && strcmp(state.GridVisible, 'off')
    vis = 'off';
  end
end
%=============================================================================
function [handles, gridLines, gridLabels] = drawPolarGrid(ax, state, maxRadius, gridVisible)
  thetaLim = thetaValuesToRadians(state.ThetaLim, state.ThetaAxisUnits);
  theta = linspace(thetaLim(1), thetaLim(2), 361);
  % Radial rings follow RColor, theta spokes follow ThetaColor; the line is the
  % axis color blended toward white, labels keep the axis color itself.
  rColor = polarGridColor(state, 'RColor');
  thetaColor = polarGridColor(state, 'ThetaColor');
  rLineColor = polarGridLineColor(rColor);
  thetaLineColor = polarGridLineColor(thetaColor);
  rLineVisible = polarComponentVisible(gridVisible, state, 'RGrid');
  thetaLineVisible = polarComponentVisible(gridVisible, state, 'ThetaGrid');
  gridLines = [drawRadialGridLines(ax, state, theta, rLineColor, rLineVisible); ...
    drawThetaGridLines(ax, state, maxRadius, thetaLineColor, thetaLineVisible)];
  % Major tick strokes in the axis color, shown with the matching grid component.
  gridLines = [gridLines; ...
    drawRadialMajorTicks(ax, state, maxRadius, rColor, rLineVisible); ...
    drawThetaMajorTicks(ax, state, maxRadius, thetaColor, thetaLineVisible)];
  % Minor grid lines, lighter than the major ones, when the minor grid is on.
  rMinorVisible = polarComponentVisible(gridVisible, state, 'RMinorGrid');
  thetaMinorVisible = polarComponentVisible(gridVisible, state, 'ThetaMinorGrid');
  rMinorColor = polarMinorLineColor(rLineColor);
  thetaMinorColor = polarMinorLineColor(thetaLineColor);
  if strcmp(rMinorVisible, 'on')
    gridLines = [gridLines; ...
      drawRadialMinorGridLines(ax, state, theta, rMinorColor, rMinorVisible)];
  end
  if strcmp(thetaMinorVisible, 'on')
    gridLines = [gridLines; ...
      drawThetaMinorGridLines(ax, state, maxRadius, thetaMinorColor, thetaMinorVisible)];
  end
  % Minor tick marks, short strokes in the axis color, toggled by RMinorTick /
  % ThetaMinorTick (distinct from the full-length minor grid lines above).
  rTickVisible = polarComponentVisible(gridVisible, state, 'RMinorTick');
  thetaTickVisible = polarComponentVisible(gridVisible, state, 'ThetaMinorTick');
  if strcmp(rTickVisible, 'on')
    gridLines = [gridLines; ...
      drawRadialMinorTicks(ax, state, maxRadius, rColor, rTickVisible)];
  end
  if strcmp(thetaTickVisible, 'on')
    gridLines = [gridLines; ...
      drawThetaMinorTicks(ax, state, maxRadius, thetaColor, thetaTickVisible)];
  end
  gridLabels = [drawRLabels(ax, state, maxRadius, rColor, gridVisible); ...
    drawThetaLabels(ax, state, maxRadius, thetaColor, gridVisible)];
  handles = [gridLines; gridLabels];
end
%=============================================================================
function color = polarGridColor(state, name)
  color = [33 33 33] / 255;
  if isfield(state, name) && numel(state.(name)) == 3
    color = state.(name)(:)';
  end
end
%=============================================================================
function lineColor = polarGridLineColor(axisColor)
  % Blend the axis color toward white so the default axis color keeps the
  % historical light grey grid [0.75 0.75 0.75].
  defaultGray = 33 / 255;
  alpha = 0.25 / (1 - defaultGray);
  lineColor = axisColor * alpha + (1 - alpha);
  lineColor = min(max(lineColor, 0), 1);
end
%=============================================================================
function visible = polarComponentVisible(gridVisible, state, name)
  visible = gridVisible;
  if strcmp(gridVisible, 'off')
    return
  end
  if isfield(state, name) && strcmp(char(state.(name)), 'off')
    visible = 'off';
  end
end
%=============================================================================
function handle = drawRadialGridLines(ax, state, theta, color, gridVisible)
  gridX = [];
  gridY = [];
  angle = polarApplyConvention(state, theta);
  for k = 1:numel(state.RTick)
    r = state.RTick(k);
    if r < state.RLim(1) || r > state.RLim(2)
      continue
    end
    rho = polarRadialValue(state, r);
    [x, y] = polarToCartesian(angle, rho * ones(size(angle)));
    gridX = [gridX, x, NaN];
    gridY = [gridY, y, NaN];
  end
  handle = drawGridLine(ax, gridX, gridY, color, gridVisible);
end
%=============================================================================
function handle = drawThetaGridLines(ax, state, maxRadius, color, gridVisible)
  ticks = visibleThetaTicks(state);
  x = NaN(1, 3 * numel(ticks));
  y = x;
  for k = 1:numel(ticks)
    value = ticks(k);
    th = polarApplyConvention(state, thetaValueToRadians(value, state.ThetaAxisUnits));
    offset = 3 * (k - 1);
    x(offset + 1:offset + 3) = [0 maxRadius * cos(th) NaN];
    y(offset + 1:offset + 3) = [0 maxRadius * sin(th) NaN];
  end
  handle = drawGridLine(ax, x, y, color, gridVisible);
end
%=============================================================================
function handle = drawGridLine(ax, x, y, color, gridVisible)
  if isempty(x)
    handle = gobjects(0, 1);
    return
  end
  handle = __line__('Parent', ax, 'XData', x, 'YData', y, ...
    'Color', color, 'LineStyle', '-', 'LineWidth', 0.5, ...
    'HandleVisibility', 'off', 'HitTest', 'off', ...
    'Tag', '__nelson_polar_grid__', 'Visible', gridVisible);
end
%=============================================================================
function handles = drawRLabels(ax, state, maxRadius, color, gridVisible)
  handles = gobjects(min(numel(state.RTick), numel(state.RTickLabel)), 1);
  count = 0;
  labelAngle = polarRAxisLocationAngle(state);
  labels = state.RTickLabel;
  for k = 1:min(numel(state.RTick), numel(labels))
    r = state.RTick(k);
    if r < state.RLim(1) || r > state.RLim(2)
      continue
    end
    angle = polarApplyConvention(state, labelAngle);
    rho = polarRadialValue(state, r);
    x = rho * cos(angle);
    y = rho * sin(angle);
    count = count + 1;
    handles(count, 1) = __text__('Parent', ax, 'Visible', gridVisible, ...
      'Position', [x y 0], 'String', labels{k}, 'Color', color, ...
      'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', ...
      'HandleVisibility', 'off', 'HitTest', 'off', ...
      'Tag', '__nelson_polar_grid__');
  end
  if count == 0
    handles = gobjects(0, 1);
  else
    handles = handles(1:count);
  end
end
%=============================================================================
function handles = drawThetaLabels(ax, state, maxRadius, color, gridVisible)
  handles = gobjects(min(numel(state.ThetaTick), numel(state.ThetaTickLabel)), 1);
  count = 0;
  labels = state.ThetaTickLabel;
  radius = maxRadius * 1.06;
  for k = 1:min(numel(state.ThetaTick), numel(labels))
    value = state.ThetaTick(k);
    if value < state.ThetaLim(1) || value > state.ThetaLim(2)
      continue
    end
    th = polarApplyConvention(state, thetaValueToRadians(value, state.ThetaAxisUnits));
    count = count + 1;
    handles(count, 1) = __text__('Parent', ax, 'Visible', gridVisible, ...
      'Position', [radius * cos(th) radius * sin(th) 0], ...
      'String', labels{k}, 'Color', color, ...
      'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', ...
      'HandleVisibility', 'off', 'HitTest', 'off', ...
      'Tag', '__nelson_polar_grid__');
  end
  if count == 0
    handles = gobjects(0, 1);
  else
    handles = handles(1:count);
  end
end
%=============================================================================
function reorderPolarChildren(ax, state, gridLines, gridLabels)
  data = state.DataHandles(:);
  known = [gridLines(:); gridLabels(:); data(:)];
  children = allchild(ax);
  if numel(children) == numel(known)
    other = gobjects(0, 1);
  else
    other = handlesNotIn(children, known);
  end
  % Built back to front: the grid sits under the data, the labels over it.
  ordered = [gridLines(:); data(:); other(:); gridLabels(:)];
  if ~isempty(ordered)
    % Children run top first, so the stack is handed back the other way up.
    ax.Children = flipud(ordered);
  end
end
%=============================================================================
function selected = handlesNotIn(handles, excluded)
  selected = gobjects(0, 1);
  for k = 1:numel(handles)
    if ~isKnownHandle(handles(k), excluded)
      selected(end + 1, 1) = handles(k);
    end
  end
end
%=============================================================================
function tf = isKnownHandle(handle, handles)
  tf = false;
  for k = 1:numel(handles)
    if isequal(handles(k), handle)
      tf = true;
      return
    end
  end
end
%=============================================================================
function radians = deg2radLocal(degrees)
  radians = degrees * pi / 180;
end
%=============================================================================
function ticks = automaticThetaTicks(thetaLim, thetaAxisUnits)
  step = 30;
  if strcmp(thetaAxisUnits, 'radians')
    step = pi / 6;
  end
  ticks = thetaLim(1):step:thetaLim(2);
  if isempty(ticks) || ticks(end) < thetaLim(2)
    ticks = [ticks thetaLim(2)];
  end
end
%=============================================================================
function ticks = visibleThetaTicks(state)
  ticks = state.ThetaTick;
  ticks = ticks(ticks >= state.ThetaLim(1) & ticks <= state.ThetaLim(2));
  if isFullCircle(state.ThetaLim, state.ThetaAxisUnits) && numel(ticks) > 1 ...
      && abs(ticks(end) - state.ThetaLim(2)) < 1e-12
    ticks(end) = [];
  end
end
%=============================================================================
function tf = isFullCircle(thetaLim, thetaAxisUnits)
  fullCircle = 360;
  if strcmp(thetaAxisUnits, 'radians')
    fullCircle = 2 * pi;
  end
  tf = abs(abs(thetaLim(2) - thetaLim(1)) - fullCircle) < 1e-12;
end
%=============================================================================
function radians = thetaValuesToRadians(values, thetaAxisUnits)
  radians = values;
  if strcmp(thetaAxisUnits, 'degrees')
    radians = values * pi / 180;
  end
end
%=============================================================================
function radians = thetaValueToRadians(value, thetaAxisUnits)
  radians = value;
  if strcmp(thetaAxisUnits, 'degrees')
    radians = deg2radLocal(value);
  end
end
%=============================================================================
function angle = polarApplyConvention(state, theta)
  % Map a data angle (radians) to a plot angle: angle = zeroOffset + dirSign * theta.
  angle = polarZeroOffset(state) + polarDirSign(state) .* theta;
end
%=============================================================================
function offset = polarZeroOffset(state)
  offset = 0;
  if ~isfield(state, 'ThetaZeroLocation')
    return
  end
  switch char(state.ThetaZeroLocation)
    case 'top'
      offset = pi / 2;
    case 'left'
      offset = pi;
    case 'bottom'
      offset = -pi / 2;
    otherwise
      offset = 0;
  end
end
%=============================================================================
function sign = polarDirSign(state)
  sign = 1;
  if isfield(state, 'ThetaDir') && strcmp(char(state.ThetaDir), 'clockwise')
    sign = -1;
  end
end
%=============================================================================
function rho = polarRadialValue(state, r)
  % Geometric radius of a data value; RDir 'reverse' mirrors it about RLim.
  rho = r;
  if isfield(state, 'RDir') && strcmp(char(state.RDir), 'reverse')
    rho = state.RLim(1) + state.RLim(2) - r;
  end
end
%=============================================================================
function angle = polarRAxisLocationAngle(state)
  % Angle (radians) along which the radial tick labels are placed.
  angle = deg2radLocal(80);
  if isfield(state, 'RAxisLocation') && isscalar(state.RAxisLocation) ...
      && isfinite(state.RAxisLocation)
    angle = deg2radLocal(state.RAxisLocation);
  end
end
%=============================================================================
function color = polarMinorLineColor(majorLineColor)
  % Minor grid lines sit halfway between the major line color and white.
  color = (majorLineColor + 1) / 2;
  color = min(max(color, 0), 1);
end
%=============================================================================
function handle = drawRadialMinorGridLines(ax, state, theta, color, gridVisible)
  gridX = [];
  gridY = [];
  angle = polarApplyConvention(state, theta);
  ticks = polarMinorRadialTicks(state);
  for k = 1:numel(ticks)
    rho = polarRadialValue(state, ticks(k));
    [x, y] = polarToCartesian(angle, rho * ones(size(angle)));
    gridX = [gridX, x, NaN];
    gridY = [gridY, y, NaN];
  end
  handle = drawGridLine(ax, gridX, gridY, color, gridVisible);
end
%=============================================================================
function handle = drawThetaMinorGridLines(ax, state, maxRadius, color, gridVisible)
  ticks = polarMinorThetaTicks(state);
  x = NaN(1, 3 * numel(ticks));
  y = x;
  for k = 1:numel(ticks)
    th = polarApplyConvention(state, thetaValueToRadians(ticks(k), state.ThetaAxisUnits));
    offset = 3 * (k - 1);
    x(offset + 1:offset + 3) = [0 maxRadius * cos(th) NaN];
    y(offset + 1:offset + 3) = [0 maxRadius * sin(th) NaN];
  end
  handle = drawGridLine(ax, x, y, color, gridVisible);
end
%=============================================================================
function handle = drawThetaMajorTicks(ax, state, maxRadius, color, gridVisible)
  % Short radial strokes at the outer rim, one per major theta tick.
  ticks = visibleThetaTicks(state);
  tickLength = maxRadius * 0.05;
  x = NaN(1, 3 * numel(ticks));
  y = x;
  for k = 1:numel(ticks)
    th = polarApplyConvention(state, thetaValueToRadians(ticks(k), state.ThetaAxisUnits));
    offset = 3 * (k - 1);
    x(offset + 1:offset + 3) = [(maxRadius - tickLength) * cos(th) maxRadius * cos(th) NaN];
    y(offset + 1:offset + 3) = [(maxRadius - tickLength) * sin(th) maxRadius * sin(th) NaN];
  end
  handle = drawGridLine(ax, x, y, color, gridVisible);
end
%=============================================================================
function handle = drawRadialMajorTicks(ax, state, maxRadius, color, gridVisible)
  % Short cross strokes along the radial axis, one per major radial tick.
  angle = polarApplyConvention(state, polarRAxisLocationAngle(state));
  halfLength = maxRadius * 0.03;
  px = -sin(angle);
  py = cos(angle);
  ticks = state.RTick;
  ticks = ticks(ticks >= state.RLim(1) & ticks <= state.RLim(2));
  x = NaN(1, 3 * numel(ticks));
  y = x;
  for k = 1:numel(ticks)
    rho = polarRadialValue(state, ticks(k));
    cx = rho * cos(angle);
    cy = rho * sin(angle);
    offset = 3 * (k - 1);
    x(offset + 1:offset + 3) = [cx - halfLength * px, cx + halfLength * px, NaN];
    y(offset + 1:offset + 3) = [cy - halfLength * py, cy + halfLength * py, NaN];
  end
  handle = drawGridLine(ax, x, y, color, gridVisible);
end
%=============================================================================
function handle = drawThetaMinorTicks(ax, state, maxRadius, color, gridVisible)
  % Short radial strokes at the outer rim, one per minor theta angle.
  ticks = polarMinorThetaTicks(state);
  tickLength = maxRadius * 0.03;
  x = NaN(1, 3 * numel(ticks));
  y = x;
  for k = 1:numel(ticks)
    th = polarApplyConvention(state, thetaValueToRadians(ticks(k), state.ThetaAxisUnits));
    offset = 3 * (k - 1);
    x(offset + 1:offset + 3) = [(maxRadius - tickLength) * cos(th) maxRadius * cos(th) NaN];
    y(offset + 1:offset + 3) = [(maxRadius - tickLength) * sin(th) maxRadius * sin(th) NaN];
  end
  handle = drawGridLine(ax, x, y, color, gridVisible);
end
%=============================================================================
function handle = drawRadialMinorTicks(ax, state, maxRadius, color, gridVisible)
  % Short cross strokes along the radial axis, one per minor radial tick.
  ticks = polarMinorRadialTicks(state);
  angle = polarApplyConvention(state, polarRAxisLocationAngle(state));
  halfLength = maxRadius * 0.02;
  px = -sin(angle);
  py = cos(angle);
  x = NaN(1, 3 * numel(ticks));
  y = x;
  for k = 1:numel(ticks)
    rho = polarRadialValue(state, ticks(k));
    cx = rho * cos(angle);
    cy = rho * sin(angle);
    offset = 3 * (k - 1);
    x(offset + 1:offset + 3) = [cx - halfLength * px, cx + halfLength * px, NaN];
    y(offset + 1:offset + 3) = [cy - halfLength * py, cy + halfLength * py, NaN];
  end
  handle = drawGridLine(ax, x, y, color, gridVisible);
end
%=============================================================================
function ticks = polarMinorRadialTicks(state)
  % Four minor ticks inside each major radial interval.
  ticks = [];
  major = state.RTick;
  major = major(major >= state.RLim(1) & major <= state.RLim(2));
  for k = 1:numel(major) - 1
    seg = linspace(major(k), major(k + 1), 6);
    ticks = [ticks, seg(2:end - 1)];
  end
end
%=============================================================================
function ticks = polarMinorThetaTicks(state)
  % Two minor spokes inside each major theta interval.
  ticks = [];
  major = state.ThetaTick;
  major = major(major >= state.ThetaLim(1) & major <= state.ThetaLim(2));
  for k = 1:numel(major) - 1
    seg = linspace(major(k), major(k + 1), 4);
    ticks = [ticks, seg(2:end - 1)];
  end
end
%=============================================================================
