%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = plot(varargin)
  inputArguments = convertStringToCharArgs(varargin);
  nbInputArguments = nargin;
  go = [];
  if (nbInputArguments >= 2) && (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    go = inputArguments{1}(1);
    inputArguments(1) = [];
    nbInputArguments = nbInputArguments - 1;
  end

  propertyIndex = 0;
  if (nbInputArguments > 2)
    propertyIndex = nbInputArguments - 1;
    bContinue = 1;
    while ((propertyIndex >= 1) && bContinue)
      bContinue = isLineProperty(inputArguments{propertyIndex});
      if bContinue
        propertyIndex = propertyIndex - 2;
      end
    end
    propertyIndex = propertyIndex + 2;
  end

  propertiesList = {};
  if ((propertyIndex > 0) & (propertyIndex < nbInputArguments))
    propertiesList = inputArguments(propertyIndex:end);
    inputArguments(propertyIndex:end) = [];
    nbInputArguments = length(inputArguments);
  end
  [go, propertiesList] = extractParentProperty(go, propertiesList);

  keepBox = false;
  if isempty(go)
    fig = groot().CurrentFigure;
    if ~isempty(fig) && ~isempty(fig.CurrentAxes)
      keepBox = strcmp(fig.CurrentAxes.NextPlot, 'add');
    end
    go = newplot();
  elseif isgraphics(go, 'axes')
    keepBox = strcmp(go.NextPlot, 'add');
    go = newplot(go);
  else
    if ~isgraphics(go, 'hggroup')
      error('Nelson:graphics:invalidParent', 'Parent must be an axes or hggroup object.');
    end
    ax = ancestor(go, 'axes');
    keepBox = strcmp(ax.NextPlot, 'add');
  end
  if isgraphics(go, 'hggroup')
    ax = ancestor(go, 'axes');
  else
    ax = go;
  end
  if ~keepBox && ~isUiFigureAxes(ax)
    ax.Box = 'on';
  end

  h = [];
  while (~isempty(inputArguments))
    cs = '';
    ms = '';
    ps = '';
    if (length(inputArguments) == 1)
      if isempty(h)
        h = plot_Y(inputArguments{1}, go, propertiesList);
      else
        h = [h; plot_Y(inputArguments{1}, go, propertiesList)];
      end
      inputArguments(1) = [];
    else
      [ps, cs, ms, msg] = colstyle(inputArguments{2}, 'plot', false);
      r = isempty(msg);
      if (r)
        h = [h; plot_Y(inputArguments{1}, go, completeProperties(cs, ms, ps, propertiesList))];
        inputArguments(1:2) = [];
      elseif (length(inputArguments) == 2)
        if isempty(h)
          h = plot_XY(inputArguments{1}, inputArguments{2}, go, propertiesList);
        else
          h = [h; plot_XY(inputArguments{1}, inputArguments{2}, go, propertiesList)];
        end
        inputArguments(1:2) = [];
      else
        [ps, cs, ms, msg] = colstyle(inputArguments{3}, 'plot', false);
        r = isempty(msg);
        if (r)
          h = [h; plot_XY(inputArguments{1}, inputArguments{2}, go, completeProperties(cs, ms, ps, propertiesList))];
          inputArguments(1:3) = [];
        else
          if (isempty(h))
            h = plot_XY(inputArguments{1}, inputArguments{2}, go, propertiesList);
          else
            h = [h; plot_XY(inputArguments{1}, inputArguments{2}, go, propertiesList)];
          end
          inputArguments(1:2) = [];
        end
      end
    end
  end
  applyUiAxesPlotTicks(ax);
  if isempty(find(strcmp(propertiesList, 'Visible')))
    h.Visible = 'on';
  end
  if (nargout > 0)
    varargout{1} = h;
  end
end
%=============================================================================
function applyUiAxesPlotTicks(ax)
  if ~isUiFigureAxes(ax)
    return
  end
  if strcmp(ax.XTickMode, 'auto')
    ticks = uiAxesNiceTicks(ax.XLim);
    if ~isempty(ticks)
      ax.XTick = ticks;
    end
  end
  if strcmp(ax.YTickMode, 'auto')
    ticks = uiAxesNiceTicks(ax.YLim);
    if ~isempty(ticks)
      ax.YTick = ticks;
    end
  end
end
%=============================================================================
function ticks = uiAxesNiceTicks(limits)
  ticks = [];
  if numel(limits) ~= 2 || any(~isfinite(limits)) || limits(1) == limits(2)
    return
  end
  span = abs(diff(limits));
  if limits(1) < 0 && limits(2) > 0 && span <= 2
    rawStep = span / 4;
  else
    rawStep = span / 10;
  end
  step = uiAxesTickStep(rawStep);
  firstTick = ceil(limits(1) / step) * step;
  lastTick = floor(limits(2) / step) * step;
  if firstTick <= lastTick
    ticks = firstTick:step:lastTick;
  end
end
%=============================================================================
function step = uiAxesTickStep(rawStep)
  if rawStep <= 0 || ~isfinite(rawStep)
    step = 1;
    return
  end
  scale = 10 ^ floor(log10(rawStep));
  normalized = rawStep / scale;
  if normalized <= 1
    step = scale;
  elseif normalized <= 2
    step = 2 * scale;
  elseif normalized <= 5
    step = 5 * scale;
  else
    step = 10 * scale;
  end
end
%=============================================================================
function tf = isUiFigureAxes(ax)
  tf = false;
  if ~isgraphics(ax, 'axes')
    return
  end
  parent = ax.Parent;
  if isempty(parent) || ~isgraphics(parent, 'figure')
    return
  end
  requiredProperties = {'MenuBar', 'ToolBar', 'NumberTitle', 'IntegerHandle'};
  for k = 1:numel(requiredProperties)
    if ~isprop(parent, requiredProperties{k})
      return
    end
  end
  tf = strcmp(parent.MenuBar, 'none') && strcmp(parent.ToolBar, 'none') ...
    && strcmp(parent.NumberTitle, 'off') && strcmp(parent.IntegerHandle, 'off');
end
%=============================================================================
function [parent, propertiesList] = extractParentProperty(parent, propertiesList)
  parentIndex = [];
  for k = 1:2:(length(propertiesList) - 1)
    if ischar(propertiesList{k}) && strcmpi(propertiesList{k}, 'Parent')
      parentIndex = k;
    end
  end
  if ~isempty(parentIndex)
    parent = propertiesList{parentIndex + 1};
    propertiesList(parentIndex:parentIndex + 1) = [];
  end
end
%=============================================================================
function q = completeProperties(cs, ms, ps, p)
  if isempty(ps)
    ps = 'none';
  end
  if (strcmp(cs, ''))
    q = {'Marker', ms, 'LineStyle', ps, p{:}};
  else
    q = {'Color', cs, 'Marker', ms, 'LineStyle', ps, p{:}};
  end
end
%=============================================================================
function k = plotVector(go, x, y, lineProperties)
  if isgraphics(go, 'hggroup')
    ax = ancestor(go, 'axes');
  else
    ax = go;
  end
  lineStyle = getLineStyleAndUpdateIndex(ax);
  color = getColorAndUpdateIndex(ax);
  if (~any(strcmp(lineProperties, 'LineStyle')))
    lineProperties = [lineProperties, {'LineStyle', lineStyle}];
  end
  k = __line__('Parent', go, 'XData', x, 'YData', y, 'Color', color, 'Visible', 'off', lineProperties{:});
end
%=============================================================================
function h = plot_XY(X, Y, go, lineProperties)
  h = [];
  [X, xIsDate] = datetimeToSerial(X);
  [Y, yIsDate] = datetimeToSerial(Y);
  [X, Y] = matchMatrix(X, Y);
  if (isvector(X))
    X = X(:);
  end;
  if (isvector(Y))
    Y = Y(:);
  end;
  for i = 1:size(Y, 2)
    h = [h; plotVector(go, X(:, i), Y(:, i), lineProperties)];
  end
  if xIsDate
    applyDatetimeAxis(go, 'x');
  end
  if yIsDate
    applyDatetimeAxis(go, 'y');
  end
end
%=============================================================================
function [a, b] = matchMatrix(a, b)
  if (isvector(a) && ~isvector(b))
    if (length(a) == size(b, 1))
      a = repmat(a(:), [1, size(b, 2)]);
      return
    else
      if (length(a) == size(b, 2))
        b = b';
        a = repmat(a(:)', [size(b, 2), 1])';
        return
      else
        error('Nelson:dimagree:dimensionsDoNotMatch', 'Dimensions do not match.');
      end
    end
  end
  if (~isvector(a) && isvector(b))
    if (length(b) == size(a, 1))
      b = repmat(b(:), [1, size(a, 2)]);
    else
      if (length(b) == size(a, 2))
        a = a';
        b = repmat(b(:)', [size(a, 2), 1])';
      else
        error('Nelson:dimagree:dimensionsDoNotMatch', 'Dimensions do not match.');
      end
    end
  end
end
%=============================================================================
function h = plot_Y(Y, go, lineProperties)
  h = [];
  [Y, yIsDate] = datetimeToSerial(Y);
  if (isvector(Y)) Y = Y(:); end;
  if (isreal(Y))
    n = 1:size(Y, 1);
    lineProperties = [lineProperties, {'XDataMode', 'auto'}];
    for i = 1:size(Y, 2)
      h = [h; plotVector(go, n, Y(:, i), lineProperties)];
    end
  else
    for i = 1:size(Y, 2)
      h = [h; plotVector(go, real(Y(:, i)), imag(Y(:, i)), lineProperties)];
    end
  end
  if yIsDate
    applyDatetimeAxis(go, 'y');
  end
end
%=============================================================================
