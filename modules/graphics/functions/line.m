%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = line(varargin)
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  if (nargin == 0)
    ax = gca();
    handle = line(ax, [0 1], [0 1]);
    ax.XLim = [0 inf];
    ax.YLim = [0 inf];
    if (nargout > 0)
      varargout{1} = handle;
    end
    return
  end
  if (length(inputArguments) > 0 && isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup') || isgraphics(inputArguments{1}, 'hgtransform')))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  [ax, inputArguments] = extractParentProperty(ax, inputArguments);

  % When the caller does not pin an explicit Color, line() draws with the next
  % color of the axes ColorOrder and advances the shared color index, matching
  % the reference (plot then line -> next color; two lines -> two colors). The
  % ColorOrder lives on the axes, so an hggroup parent resolves to its ancestor.
  colorAxes = ax;
  if isgraphics(ax, 'hggroup') || isgraphics(ax, 'hgtransform')
    colorAxes = ancestor(ax, 'axes');
  end
  needsColor = ~hasColorProperty(inputArguments) && ~isempty(colorAxes) && isgraphics(colorAxes, 'axes');

  xIsDate = false;
  yIsDate = false;
  if isempty(inputArguments)
    colorArgs = nextColorArgs(colorAxes, needsColor);
    handle = __line__('Parent', ax, colorArgs{:});
  elseif (ischar(inputArguments{1}))
    colorArgs = nextColorArgs(colorAxes, needsColor);
    handle = __line__('Parent', ax, colorArgs{:}, inputArguments{:});
  elseif (numel(inputArguments) == 2)
    [x, xIsDate] = datetimeToSerial(inputArguments{1});
    [y, yIsDate] = datetimeToSerial(inputArguments{2});
    handle = createLineColumns(ax, colorAxes, needsColor, x, y, ones(0, 1), {});
  elseif (numel(inputArguments) >= 3)
    [x, xIsDate] = datetimeToSerial(inputArguments{1});
    [y, yIsDate] = datetimeToSerial(inputArguments{2});
    inputArguments = inputArguments(3:end);
    if isnumeric(inputArguments{1})
      z = inputArguments{1};
      inputArguments = inputArguments(2:end);
    else
      z = ones(0, 1);
    end
    handle = createLineColumns(ax, colorAxes, needsColor, x, y, z, inputArguments);
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if xIsDate
    applyDatetimeAxis(ax, 'x');
  end
  if yIsDate
    applyDatetimeAxis(ax, 'y');
  end
  if (nargout > 0)
    varargout{1} = handle;
  end
end
%=============================================================================
function colorArgs = nextColorArgs(colorAxes, needsColor)
  if needsColor
    colorArgs = {'Color', getColorAndUpdateIndex(colorAxes)};
  else
    colorArgs = {};
  end
end
%=============================================================================
function handle = createLineColumns(ax, colorAxes, needsColor, x, y, z, extraArgs)
  % When the data are matrices, line draws one object per column (a vector is a
  % single line and is shared across the columns of a matrix partner), matching
  % the reference. Each column takes the next color of the axes ColorOrder when
  % no explicit Color was given.
  n = max([lineColumnCount(x), lineColumnCount(y), lineColumnCount(z)]);
  has3D = ~isempty(z);
  handle = gobjects(n, 1);
  for k = 1:n
    xk = lineColumn(x, k);
    yk = lineColumn(y, k);
    colorArgs = nextColorArgs(colorAxes, needsColor);
    if has3D
      zk = lineColumn(z, k);
      handle(k) = __line__('Parent', ax, 'XData', xk, 'YData', yk, 'ZData', zk, ...
        colorArgs{:}, extraArgs{:});
    else
      handle(k) = __line__('Parent', ax, 'XData', xk, 'YData', yk, ...
        colorArgs{:}, extraArgs{:});
    end
  end
end
%=============================================================================
function n = lineColumnCount(v)
  if isempty(v) || isvector(v)
    n = 1;
  else
    n = size(v, 2);
  end
end
%=============================================================================
function col = lineColumn(v, k)
  if isempty(v) || isvector(v)
    col = v;
  else
    col = v(:, k);
  end
end
%=============================================================================
function tf = hasColorProperty(inputArguments)
  tf = false;
  for k = 1:length(inputArguments)
    if ischar(inputArguments{k}) && strcmpi(inputArguments{k}, 'Color')
      tf = true;
      return
    end
  end
end
%=============================================================================
function [parent, inputArguments] = extractParentProperty(parent, inputArguments)
  parentIndex = [];
  for k = 1:(length(inputArguments) - 1)
    if ischar(inputArguments{k}) && strcmpi(inputArguments{k}, 'Parent')
      parentIndex = k;
    end
  end
  if ~isempty(parentIndex)
    parent = inputArguments{parentIndex + 1};
    inputArguments(parentIndex:parentIndex + 1) = [];
  end
end
%=============================================================================
