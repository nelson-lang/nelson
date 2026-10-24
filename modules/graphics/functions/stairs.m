%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = stairs(varargin)
  nargoutchk(0, 2);
  if (nargout == 2)
    narginchk(1, 2);
    [xb, yb] = stairsImplementation(varargin{:});
    varargout{1} = xb;
    varargout{2} = yb;
  else
    narginchk(1, 20);
    h = stairsPlot(varargin{:});
    if (nargout == 1)
      varargout{1} = h;
    end
  end
end
%=============================================================================
function [XB, YB] = stairsImplementation(varargin)
  [X, Y] = prepareXY(varargin{:});

  [m, n] = size(Y);
  if ((m == 1) && (n > 1))
    X = X';
    Y = Y';
    [m, n] = size(Y);
  end

  r = 2 * m - 1;
  c = n;

  XB = zeros(r, c);
  YB = XB;

  idxA = 1:2:2 * m;
  idxB = 2:2:2 * (m - 1);
  XB(idxA, :) = X;
  XB(idxB, :) = X(2:end, :);

  idxA = 1:2:r;
  YB(idxA, :) = Y;
  YB(idxB, :) = Y(1:(end - 1), :);
end
%=============================================================================
function [X, Y] = prepareXY(varargin)
  if (nargin == 1)
    Y = varargin{1};
    if isvector(Y)
      X = 1:length(Y);
      X = reshape(X, size(Y));
    else
      [m, n] = size(Y);
      X = [1:m]';
      X = repmat(X, 1, n);
    end
  else
    Y = varargin{2};
    if ~ismatrix(Y)
      error('Nelson:stairs:invalidY', 'Y must be a vector or matrix.');
    end
    X = varargin{1};

    if (isvector(X) && isvector(Y))
      X = reshape(X, length(X), 1);
      Y = reshape(Y, length(Y), 1);
    elseif (isvector(X) && ~isvector(Y))
      if (isrow(X))
        error('Nelson:dimagree:dimensionsDoNotMatch', 'Dimensions do not match.');
      end
      [mx, nx] = size(X);
      [my, ny] = size(Y);
      if ((nx == 1) && (mx == my))
        X = repmat(X, 1, ny);
      end
    elseif (~isvector(X) && isvector(Y))
      [mx, nx] = size(X);
      [my, ny] = size(Y);
      if ((ny == 1) && (mx == my))
        Y = repmat(Y, 1, nx);
      end
    end

    if (~isempty(find([size(X) == size(Y)] == 0)))
      error('Nelson:dimagree:dimensionsDoNotMatch', 'Dimensions do not match.');
    end
  end
end
%=============================================================================
function h = stairsPlot(varargin)
  inputArguments = varargin;
  nbInputArguments = length(inputArguments);
  if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    parent = newplotTarget(inputArguments{1});
    inputArguments = inputArguments(2:end);
    nbInputArguments = nbInputArguments - 1;
  else
    [parent, inputArguments] = graphicsExtractParentProperty(inputArguments);
    nbInputArguments = length(inputArguments);
    parent = newplotTarget(parent);
  end
  applyStairsAxesBox(parent);
  xIsDate = false;
  yIsDate = false;
  if ~isempty(inputArguments)
    [inputArguments{1}, xIsDate] = datetimeToSerial(inputArguments{1});
  end
  if nbInputArguments > 1
    [inputArguments{2}, yIsDate] = datetimeToSerial(inputArguments{2});
  end
  % The stair object stores the RAW data points and builds the staircase at
  % paint time (matching expected behavior), so we pass X/Y through prepareXY only.
  % xAuto marks the single-argument form where X is the auto sample index.
  if nbInputArguments > 1 && isnumeric(inputArguments{1}) && isnumeric(inputArguments{2})
    X = inputArguments{1};
    Y = inputArguments{2};
    inputArguments = inputArguments(3:end);
    [XB, YB] = prepareXY(X, Y);
    xAuto = false;
  else
    % Single data argument: the lone series is the Y axis, so a datetime
    % detected as the first argument actually maps to the Y axis.
    yIsDate = xIsDate;
    xIsDate = false;
    if isnumeric(inputArguments{1})
      Y = inputArguments{1};
      inputArguments = inputArguments(2:end);
      [XB, YB] = prepareXY(Y);
      xAuto = true;
    else
      error('Nelson:stairs:wrongNumberOfOutputArguments', 'Wrong number of output arguments.');
    end
  end
  h = stairsLinePlot(parent, XB, YB, inputArguments, xAuto);
  if xIsDate
    applyDatetimeAxis(parent, 'x');
  end
  if yIsDate
    applyDatetimeAxis(parent, 'y');
  end
end
%=============================================================================
function applyStairsAxesBox(parent)
  if isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = parent;
  end
  if ~isempty(ax) && ~strcmp(ax.NextPlot, 'add') && ~isStairsUiFigureAxes(ax)
    ax.Box = 'on';
  end
end
%=============================================================================
function tf = isStairsUiFigureAxes(ax)
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
function h = stairsLinePlot(parent, X, Y, inputArguments, xAuto)
  [lineSpec, lineProperties, visibleExplicit] = parseStairsLineArguments(inputArguments);
  if ~isempty(lineSpec)
    [lineStyle, colorSpec, markerSpec, msg] = colstyle(lineSpec, 'plot', false);
    if ~isempty(msg)
      error('Nelson:stairs:invalidLineSpec', msg.message);
    end
    lineProperties = completeStairsLineProperties(colorSpec, markerSpec, lineStyle, lineProperties);
  end
  h = stairsPlotXY(X, Y, parent, lineProperties, xAuto);
  if ~visibleExplicit
    h.Visible = 'on';
  end
end
%=============================================================================
function [lineSpec, propertiesList, visibleExplicit] = parseStairsLineArguments(inputArguments)
  lineSpec = '';
  visibleExplicit = false;
  propertyIndex = 0;
  nbInputArguments = length(inputArguments);
  if (nbInputArguments >= 2)
    propertyIndex = nbInputArguments - 1;
    keepScanning = true;
    while ((propertyIndex >= 1) && keepScanning)
      keepScanning = ischar(inputArguments{propertyIndex}) ...
        && isValidGraphicsProperty('line', inputArguments{propertyIndex});
      if keepScanning
        propertyIndex = propertyIndex - 2;
      end
    end
    propertyIndex = propertyIndex + 2;
  end
  propertiesList = {};
  if ((propertyIndex > 0) && (propertyIndex <= nbInputArguments))
    propertiesList = inputArguments(propertyIndex:end);
    inputArguments(propertyIndex:end) = [];
  end
  if (length(inputArguments) > 1)
    error('Nelson:stairs:invalidLineSpec', 'Invalid LineSpec string.');
  end
  if ~isempty(inputArguments)
    lineSpec = inputArguments{1};
  end
  for k = 1:2:(length(propertiesList) - 1)
    if ischar(propertiesList{k}) && strcmpi(propertiesList{k}, 'Visible')
      visibleExplicit = true;
    end
  end
end
%=============================================================================
function q = completeStairsLineProperties(colorSpec, markerSpec, lineStyle, propertiesList)
  if isempty(lineStyle)
    lineStyle = 'none';
  end
  if strcmp(colorSpec, '')
    q = {'Marker', markerSpec, 'LineStyle', lineStyle, propertiesList{:}};
  else
    q = {'Color', colorSpec, 'Marker', markerSpec, 'LineStyle', lineStyle, propertiesList{:}};
  end
end
%=============================================================================
function h = stairsPlotXY(X, Y, parent, lineProperties, xAuto)
  h = [];
  if (isvector(X))
    X = X(:);
  end
  if (isvector(Y))
    Y = Y(:);
  end
  for i = 1:size(Y, 2)
    h = [h; stairsPlotVector(parent, X(:, i), Y(:, i), lineProperties, xAuto)];
  end
end
%=============================================================================
function h = stairsPlotVector(parent, x, y, lineProperties, xAuto)
  if isgraphics(parent, 'hggroup')
    ax = ancestor(parent, 'axes');
  else
    ax = parent;
  end
  lineStyle = getLineStyleAndUpdateIndex(ax);
  color = getColorAndUpdateIndex(ax);
  if (~any(strcmp(lineProperties, 'LineStyle')))
    lineProperties = [lineProperties, {'LineStyle', lineStyle}];
  end
  % XDataMode is 'auto' only when X is the auto sample index (single-argument
  % form); Y is always supplied, so YDataMode is 'manual'.
  if xAuto
    xMode = 'auto';
  else
    xMode = 'manual';
  end
  h = __stair__('Parent', parent, 'XData', x, 'YData', y, 'Color', color, ...
    'XDataMode', xMode, 'YDataMode', 'manual', 'Visible', 'off', lineProperties{:});
end
%=============================================================================
