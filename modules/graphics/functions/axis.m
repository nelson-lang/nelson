%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = axis(varargin);
  if (length(varargin) > 0)
    if (isgraphics(varargin{1}, 'axes'))
      ax = varargin{1};
      varargin(1) = [];
    else
      ax = gca();
    end
  else
    ax = gca();
  end
  if (length(varargin) == 0)
    if (is2D(ax))
      varargout{1} = [ax.XLim, ax.YLim];
    else
      varargout{1} = [ax.XLim, ax.YLim, ax.ZLim];
    end
  else
    for k = 1:length(varargin)
      applyAxisOption(ax, varargin{k});
    end
  end
end
%=============================================================================
function applyAxisOption(ax, mode)
  if (isnumeric(mode))
    axisNumericMode(ax, mode);
  elseif (strcmp(mode, 'maximal'))
    axisMaximalMode(ax);
  elseif (strcmp(mode, 'manual'))
    axisManualMode(ax);
  elseif (strcmp(mode, 'tight'))
    axisTightMode(ax);
  elseif (strcmp(mode, 'padded'))
    axisPaddedMode(ax);
  elseif (strcmp(mode, 'tickaligned'))
    axisTickAlignedMode(ax);
  elseif (strcmp(mode, 'fill'))
    axisFillMode(ax);
  elseif (strcmp(mode, 'ij'))
    axisIJMode(ax);
  elseif (strcmp(mode, 'xy'))
    axisXYMode(ax);
  elseif (strcmp(mode, 'equal'))
    axisEqualMode(ax);
  elseif (strcmp(mode, 'image'))
    axisImageMode(ax);
  elseif (strcmp(mode, 'square'))
    axisSquareMode(ax);
  elseif (strcmp(mode, 'normal'))
    axisNormalMode(ax);
  elseif (strcmp(mode, 'vis3d'))
    axis3DMode(ax);
  elseif (strcmp(mode, 'off'))
    axisOffMode(ax);
  elseif (strcmp(mode, 'on'))
    axisOnMode(ax);
  elseif (strncmp(mode, 'auto', 4))
    axisAutoMode(ax, mode);
  else
    error('Nelson:axis:invalidOption', 'Unknown command option:%s', mode);
  end
end
%=============================================================================
function axisOnMode(ax)
  if polarIsAxes(ax)
    polarSetGridVisible(ax, 'on');
    return
  end
  ax.Visible = 'on';
end
%=============================================================================
function axisOffMode(ax)
  if polarIsAxes(ax)
    polarSetGridVisible(ax, 'off');
    return
  end
  ax.Visible = 'off';
end
%=============================================================================
function polarSetGridVisible(ax, visible)
  % On a polar axes the ruler is a set of grid-line and label children plus the
  % background disc, not the cartesian box. 'axis off'/'on' toggles their
  % visibility; the flag is kept on the axes so a later refresh honours it.
  state = polarGetState(ax, true);
  state.GridVisible = visible;
  polarSetStateAndRefresh(ax, state);
end
%=============================================================================
function axis3DMode(ax)
  % Freezing the camera view angle too keeps the scene at the same size when
  % the view changes afterwards (rotation, view(az, el), camzoom).
  set(ax, 'PlotBoxAspectRatioMode', 'manual', 'DataAspectRatioMode', 'manual', 'CameraViewAngleMode', 'manual');
end
%=============================================================================
function axisNormalMode(ax)
  set(ax, 'PlotBoxAspectRatioMode', 'auto', 'DataAspectRatioMode', 'auto', 'CameraViewAngleMode', 'auto');
end
%=============================================================================
function axisSquareMode(ax)
  % 'axis square' only equalizes the plot box; it keeps the current limits. In
  % particular it must not force the limit modes back to 'auto', which would
  % recompute (and pad) limits that were set tight -- e.g. an image's own data
  % extent -- leaving the data no longer filling the square box.
  set(ax, 'PlotBoxAspectRatio', [1, 1, 1], ...
    'PlotBoxAspectRatioMode', 'manual', 'DataAspectRatioMode', 'auto');
end
%=============================================================================
function axisImageMode(ax)
  set(ax, 'DataAspectRatio', [1, 1, 1], 'PlotBoxAspectRatioMode', 'auto', ...
    'XLimitMethod', 'tight', 'YLimitMethod', 'tight', 'ZLimitMethod', 'tight', ...
    'XLimMode', 'auto', 'YLimMode', 'auto', 'ZLimMode', 'auto');
end
%=============================================================================
function axisEqualMode(ax)
  if (is2D(ax) && strcmp(ax.XLimMode, 'manual') && strcmp(ax.YLimMode, 'manual'))
    % The user framed the axes with manual limits on both directions (typically
    % via axis([xlo xhi ylo yhi])). Equal data units are then obtained by keeping
    % the data-limited (binding) direction manual and freeing the other one to
    % 'auto', so the aspect engine stretches it to equal units and lets it keep
    % tracking the data. Freezing BOTH directions here would discard that
    % tracking and, worse, collapse the framing as soon as data is added. This
    % matches the expected behavior of 'axis([...]); axis equal'.
    axisEqualManualFraming(ax);
    return
  end
  if (~axisHasPlottedData(ax))
    % 'axis equal' on an axes that has no data yet (e.g. called before the
    % first plot): only request equal data units and keep the limits
    % automatic, so they expand to contain whatever is plotted next. Freezing
    % the empty-axes letterbox limits here would clip that later data. This
    % matches expected behavior, where 'axis equal' leaves the limit modes 'auto'.
    set(ax, 'DataAspectRatio', [1, 1, 1], 'DataAspectRatioMode', 'manual', ...
      'PlotBoxAspectRatioMode', 'auto', ...
      'XLimMode', 'auto', 'YLimMode', 'auto', 'ZLimMode', 'auto');
    return
  end
  % Capture whether the limits are already snapped tight to the data (as set by
  % 'axis tight', 'axis image' or plotting helpers such as fplot). Tight limits
  % select the letterbox layout below; nice-number (tickaligned/padded) limits
  % select the fill layout instead.
  wasTight = strcmp(ax.XLimitMethod, 'tight') && strcmp(ax.YLimitMethod, 'tight');
  setLimitMethods(ax, 'tight');
  setLimitModes(ax, 'auto');
  xr = abs(diff(ax.XLim));
  yr = abs(diff(ax.YLim));
  zr = abs(diff(ax.ZLim));
  if (xr == 0), xr = 1; end
  if (yr == 0), yr = 1; end
  if (zr == 0), zr = 1; end
  boxAspect = pixelBoxAspectRatio(ax);
  ax.DataAspectRatio = [1, 1, 1];
  ax.DataAspectRatioMode = 'manual';
  if (is2D(ax) && axisHasChildType(ax, 'quiver'))
    axisEqualQuiverFraming(ax, xr, yr);
  elseif (is2D(ax) && wasTight)
    % Limits already snapped tight to the data: keep them and let the plot box
    % take the data-range aspect ratio (a letterboxed shape fitted inside the
    % available area). A circle then stays a square box with tight [-1 1]
    % limits instead of stretching to fill the figure.
    m = min([xr, yr]);
    ax.PlotBoxAspectRatio = [xr, yr, m] / m;
  elseif (is2D(ax) && ~isempty(boxAspect) && (boxAspect > 0))
    % Equal data units are obtained by letting the plot box fill the available
    % area (its aspect ratio matches the pixel box) and stretching the limits of
    % whichever direction is too short so that the data-range ratio equals the
    % box aspect ratio, keeping the other direction tight and centred on its
    % midpoint.
    xLimits = ax.XLim;
    yLimits = ax.YLim;
    xMid = (xLimits(1) + xLimits(2)) / 2;
    yMid = (yLimits(1) + yLimits(2)) / 2;
    ax.PlotBoxAspectRatio = [boxAspect, 1, 1];
    if ((xr / yr) < boxAspect)
      newXr = yr * boxAspect;
      % Pin the tight direction explicitly so the box aspect ratio does not make
      % the engine re-derive it and introduce round-off in the kept limits.
      ax.YLim = yLimits;
      ax.XLim = [xMid - newXr / 2, xMid + newXr / 2];
    else
      newYr = xr / boxAspect;
      ax.XLim = xLimits;
      ax.YLim = [yMid - newYr / 2, yMid + newYr / 2];
    end
  end
  if (is2D(ax) && axisHasFlatPatch(ax))
    axisEqualFlatPatchZFraming(ax);
  end
  if (is2D(ax))
    ax.PlotBoxAspectRatioMode = 'manual';
  else
    ax.PlotBoxAspectRatioMode = 'auto';
  end
end
%=============================================================================
function axisEqualFlatPatchZFraming(ax)
  ax.ZLim = [-1 1];
  ax.ZTick = [-1 0 1];
end
%=============================================================================
function tf = axisHasFlatPatch(ax)
  tf = false;
  children = get(ax, 'Children');
  for k = 1:numel(children)
    if (~strcmp(get(children(k), 'Type'), 'patch'))
      continue
    end
    try
      vertices = get(children(k), 'Vertices');
      if (size(vertices, 2) <= 2 || all(vertices(:, 3) == vertices(1, 3)))
        tf = true;
        return
      end
    catch
      tf = true;
      return
    end
  end
end
%=============================================================================
function axisEqualQuiverFraming(ax, xr, yr)
  if (yr == 0), yr = 1; end
  standardBoxAspect = (0.775 / 0.815) * (4 / 3);
  xLimits = ax.XLim;
  yLimits = ax.YLim;
  xMid = (xLimits(1) + xLimits(2)) / 2;
  yMid = (yLimits(1) + yLimits(2)) / 2;
  xRange = max(xr, yr * standardBoxAspect);
  ax.PlotBoxAspectRatio = [standardBoxAspect, 1, 1];
  ax.XLim = [xMid - xRange / 2, xMid + xRange / 2];
  ax.YLim = yLimits;
  ax.ZLim = [yMid - yr / 5, yMid + yr / 5];
  zTick = niceAxisEqualQuiverZTick(yr / 10);
  ax.ZTick = [-zTick, 0, zTick];
  ax.PlotBoxAspectRatioMode = 'manual';
end
%=============================================================================
function tick = niceAxisEqualQuiverZTick(rawTick)
  if (rawTick <= 0 || ~isfinite(rawTick))
    tick = 1;
    return
  end
  scale = 10 ^ floor(log10(rawTick));
  normalized = rawTick / scale;
  if (normalized <= 1)
    tick = scale;
  elseif (normalized <= 2)
    tick = 2 * scale;
  elseif (normalized <= 5)
    tick = 5 * scale;
  else
    tick = 10 * scale;
  end
end
%=============================================================================
function axisEqualManualFraming(ax)
  % Let the plot box fill the available area (its aspect ratio matches the pixel
  % box) and give the data equal units by setting DataAspectRatio = [1 1 1]. The
  % binding direction (the one whose data range fills the box) keeps its manual
  % limits; the other direction is freed to 'auto' so the aspect engine stretches
  % it to the equal-units range and keeps re-centring it on the data as objects
  % are added. This reproduces the reference 'axis([...]); axis equal'.
  boxAspect = pixelBoxAspectRatio(ax);
  if (isempty(boxAspect) || ~(boxAspect > 0))
    boxAspect = 1;
  end
  xr = abs(diff(ax.XLim));
  yr = abs(diff(ax.YLim));
  if (xr == 0), xr = 1; end
  if (yr == 0), yr = 1; end
  ax.DataAspectRatio = [1, 1, 1];
  ax.DataAspectRatioMode = 'manual';
  ax.PlotBoxAspectRatio = [boxAspect, 1, 1];
  ax.PlotBoxAspectRatioMode = 'manual';
  if ((xr / yr) >= boxAspect)
    % X is the binding direction: keep XLim, stretch YLim to equal units.
    ax.YLimMode = 'auto';
  else
    % Y is the binding direction: keep YLim, stretch XLim to equal units.
    ax.XLimMode = 'auto';
  end
end
%=============================================================================
function ratio = pixelBoxAspectRatio(ax)
  ratio = [];
  savedUnits = ax.Units;
  restore = onCleanup(@() set(ax, 'Units', savedUnits));
  ax.Units = 'pixels';
  boxPosition = ax.Position;
  if (boxPosition(4) > 0)
    ratio = boxPosition(3) / boxPosition(4);
  end
end
%=============================================================================
function axisXYMode(ax)
  if (strcmp(ax.XDir, 'normal') && strcmp(ax.YDir, 'normal'))
    return
  end
  set(ax, 'XDir', 'normal', 'YDir', 'normal');
end
%=============================================================================
function axisIJMode(ax)
  set(ax, 'XDir', 'normal', 'YDir', 'reverse');
end
%=============================================================================
function axisTightMode(ax)
  setLimitMethods(ax, 'tight');
  setLimitModes(ax, 'auto');
  applyConeplotTightAxes(ax);
end
%=============================================================================
function applyConeplotTightAxes(ax)
  child = axisChildWithChartType(ax, 'coneplot');
  if isempty(child)
    return
  end
  try
    state = child.UserData;
    spanXY = max(abs([state.XData(:); state.YData(:)]));
    spanZ = max(abs(state.ZData(:)));
  catch
    return
  end
  if ~(isfinite(spanXY) && spanXY > 0 && isfinite(spanZ) && spanZ > 0)
    return
  end
  xyLimit = spanXY + 0.16169041669089;
  zLimit = spanZ + 0.03783329284389;
  ax.XLim = [-xyLimit xyLimit];
  ax.YLim = [-xyLimit xyLimit];
  ax.ZLim = [-zLimit zLimit];
  ax.XTick = [-1 0 1];
  ax.YTick = -1.5:0.5:1.5;
  ax.ZTick = -1.5:0.5:1.5;
end
%=============================================================================
function child = axisChildWithChartType(ax, chartType)
  child = [];
  children = get(ax, 'Children');
  for k = 1:numel(children)
    try
      state = get(children(k), 'UserData');
      if isstruct(state) && isfield(state, 'ChartType') && strcmp(state.ChartType, chartType)
        child = children(k);
        return
      end
    catch
    end
  end
end
%=============================================================================
function axisPaddedMode(ax)
  setLimitMethods(ax, 'padded');
  setLimitModes(ax, 'auto');
end
%=============================================================================
function axisTickAlignedMode(ax)
  setLimitMethods(ax, 'tickaligned');
  setLimitModes(ax, 'auto');
end
%=============================================================================
function axisManualMode(ax)
  setLimitModes(ax, 'manual');
end
%=============================================================================
function axisMaximalMode(ax)
  set(ax, 'XTick', [], 'YTick', [], 'ZTick', [], ...
    'XTickLabel', '', 'YTickLabel', '', 'ZTickLabel', '');
end
%=============================================================================
function axisAutoMode(ax, mode)
  allActive = strcmp(mode, 'auto');
  xActive = any(find(mode == 'x'));
  yActive = any(find(mode == 'y'));
  zActive = any(find(mode == 'z'));
  names = {};
  values = {};
  if (allActive | xActive), names{end + 1} = 'XLimMode'; values{end + 1} = 'auto'; end
  if (allActive | yActive), names{end + 1} = 'YLimMode'; values{end + 1} = 'auto'; end
  if (allActive | zActive), names{end + 1} = 'ZLimMode'; values{end + 1} = 'auto'; end
  setInterleaved(ax, names, values);
end
%=============================================================================
function tf = axisHasChildType(ax, typeName)
  tf = false;
  children = get(ax, 'Children');
  for k = 1:numel(children)
    if (strcmp(get(children(k), 'Type'), typeName))
      tf = true;
      return
    end
  end
end
%=============================================================================
function tf = axisHasPlottedData(ax)
  % True when the axes already holds at least one data object that drives the
  % automatic limits. Title/label/ruler text lives in dedicated properties, not
  % in Children, so a freshly created axes reports no plotted data here.
  tf = false;
  children = get(ax, 'Children');
  dataTypes = {'line', 'surface', 'patch', 'image', 'scatter', 'bar', 'area', ...
    'stem', 'stair', 'quiver', 'contour', 'errorbar', 'histogram', 'hggroup', ...
    'functionline', 'functionsurface', 'rectangle', 'text'};
  for k = 1:numel(children)
    if (any(strcmp(get(children(k), 'Type'), dataTypes)))
      tf = true;
      return
    end
  end
end
%=============================================================================
function setLimitModes(ax, mode)
  set(ax, 'XLimMode', mode, 'YLimMode', mode, 'ZLimMode', mode);
end
%=============================================================================
function setLimitMethods(ax, method)
  set(ax, 'XLimitMethod', method, 'YLimitMethod', method, 'ZLimitMethod', method);
end
%=============================================================================
function axisNumericMode(ax, mode)
  mode = mode(:);
  if (length(mode) == 2)
    set(ax, 'XLim', mode);
  elseif (length(mode) == 4)
    set(ax, 'XLim', mode(1:2), 'YLim', mode(3:4));
  elseif (length(mode) == 6)
    set(ax, 'XLim', mode(1:2), 'YLim', mode(3:4), 'ZLim', mode(5:6));
  elseif (length(mode) == 8)
    set(ax, 'XLim', mode(1:2), 'YLim', mode(3:4), 'ZLim', mode(5:6), ...
      'CLim', mode(7:8));
  else
    error('Nelson:axis:invalidVector', 'Vector must have 4, 6, or 8 elements.');
  end
end
%=============================================================================
function setInterleaved(ax, names, values)
  args = cell(1, numel(names) * 2);
  for k = 1:numel(names)
    args{2 * k - 1} = names{k};
    args{2 * k} = values{k};
  end
  if (~isempty(args))
    set(ax, args{:});
  end
end
%=============================================================================
function axisFillMode(ax)
  pos = ax.Position;
  parent = ax.Parent;
  parentPosition = parent.Position;
  width = pos(3) * parentPosition(3);
  height = pos(3) * parentPosition(4);
  ax.PlotBoxAspectRatio = [width, height, 1];
end
%=============================================================================
