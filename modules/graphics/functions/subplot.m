%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subplot(varargin)
  varargout = {};
  if ((nargin == 2) && ischar(varargin{1}) && strcmp(lower(varargin{1}), 'position'))
    ax = axes('OuterPosition', varargin{2});
    if (nargout > 0)
      varargout{1} = ax;
    end
    return;
  end
  [m, n, p, ax, reuseAx, extraArgs] = parseSubplotArguments(varargin);
  position = subplotCellPosition(m, n, p);
  [fig, reuseAx] = placeReusedAxes(reuseAx, position);
  [children, found] = clearOverlappingCells(fig, position, reuseAx);
  if (~isempty(reuseAx))
    axes(reuseAx);
    ax = reuseAx;
  elseif (~found || isempty(ax))
    ax = axes('Position', position, 'LineStyleOrder', '-', 'Box', 'off');
  end
  if (~isempty(extraArgs))
    set(ax, extraArgs{:});
  end
  if (isscalar(p) && isgraphics(ax, 'axes') && isSameGrid(children, ax, [m, n]))
    % auto FontSize of a grid cell follows the grid size
    ax.SubplotGrid_Hidden = [m, n];
  end
  fig.NextPlot = 'add';
  if (nargout > 0)
    varargout{1} = ax;
  end
end
%=============================================================================
function [m, n, p, ax, reuseAx, extraArgs] = parseSubplotArguments(args)
  m = 1;
  n = 1;
  p = 1;
  ax = [];
  reuseAx = [];
  extraArgs = {};
  nargs = numel(args);
  if (nargs == 1)
    [m, n, p, ax] = parseSingleArgument(args{1});
    return;
  end
  if (nargs >= 1)
    m = round(args{1}(1));
  end
  if (nargs >= 2)
    n = round(args{2}(1));
  end
  if (nargs >= 3)
    p = round(args{3});
    p = p(:);
  end
  if (nargs >= 4)
    [reuseAx, extraArgs] = parseFourthArgument(args);
  end
end
%=============================================================================
function [m, n, p, ax] = parseSingleArgument(arg)
  m = 1;
  n = 1;
  p = 1;
  ax = [];
  if (ischar(arg))
    if (length(arg) ~= 3)
      error('Nelson:subplot:unknownCommandOption', 'Unknown command option.');
    end
    m = double(arg(1)) - double('0');
    n = double(arg(2)) - double('0');
    p = double(arg(3)) - double('0');
  else
    axes(arg(1));
    ax = arg(1);
  end
end
%=============================================================================
function [reuseAx, extraArgs] = parseFourthArgument(args)
  reuseAx = [];
  fourth = args{4};
  if (isSubplotAxesHandle(fourth))
    % subplot(m, n, p, ax): convert the existing axes into this cell
    reuseAx = fourth;
    extraArgs = args(5:end);
    return;
  end
  % a 'replace'/'align'/'v6' option, or name/value pairs for the new axes
  extraArgs = args(4:end);
  if (ischar(extraArgs{1}) && any(strcmpi(extraArgs{1}, {'replace', 'align', 'v6'})))
    extraArgs(1) = [];
  end
end
%=============================================================================
function position = subplotCellPosition(m, n, p)
  row = round(fix((p - 1) ./ n) + 1);
  col = round(mod(p - 1, n) + 1);
  position = subplotPosition(m, n, row, col);
  if (length(p) > 1)
    position = [min(position(:, 1:2)), max(position(:, 3:4))];
  end
  position = [position(1), position(2), position(3) - position(1), position(4) - position(2)];
end
%=============================================================================
function [fig, reuseAx] = placeReusedAxes(reuseAx, position)
  if (~isempty(reuseAx))
    % place the given axes into the cell before the overlap pass so it survives
    fig = ancestor(reuseAx, 'figure');
    reuseAx.Units = 'normalized';
    reuseAx.Position = position;
  else
    fig = gcf();
  end
end
%=============================================================================
function [children, found] = clearOverlappingCells(fig, position, reuseAx)
  children = allchild(fig);
  found = false;
  childrenToDelete = [];
  for (i = 1:length(children))
    if (~isempty(reuseAx) && isequal(children(i), reuseAx))
      continue;
    end
    if (~(isgraphics(children(i), 'axes') || isgraphics(children(i), 'polaraxes')))
      continue;
    end
    currentPosition = children(i).Position;
    if (isempty(reuseAx) && all(abs(currentPosition - position) < 1e-12))
      axes(children(i));
      found = true;
    elseif (intersectionBetweenRectangles(currentPosition, position))
      childrenToDelete = [childrenToDelete, i];
    end
  end
  children(childrenToDelete) = [];
  fig.Children = children;
end
%=============================================================================
function tf = isSubplotAxesHandle(value)
  % true for a scalar axes/polaraxes/uiaxes handle passed as subplot(m, n, p, ax)
  tf = false;
  if (ischar(value) || isstring(value) || ~isscalar(value))
    return;
  end
  if (isgraphics(value, 'axes') || isgraphics(value, 'polaraxes') || isgraphics(value, 'uiaxes'))
    tf = true;
  end
end
%=============================================================================
function tf = isSameGrid(children, ax, grid)
  % a cell of another grid shape than the subplots already there is laid out alone
  tf = true;
  for i = 1:length(children)
    child = children(i);
    if isgraphics(child, 'axes') && ~isequal(child, ax)
      other = child.SubplotGrid_Hidden;
      if numel(other) == 2 && all(other >= 1) && any(other ~= grid)
        tf = false;
        return;
      end
    end
  end
end
%=============================================================================
function position = subplotPosition(m, n, row, col)
  defaultPosition = [0.13, 0.11, 0.775, 0.815];
  horizontalGapRatio = 6 / 19;
  verticalGapRatio = 7 / 18;
  width = defaultPosition(3) / (n + (n - 1) * horizontalGapRatio);
  height = defaultPosition(4) / (m + (m - 1) * verticalGapRatio);
  horizontalGap = horizontalGapRatio * width;
  verticalGap = verticalGapRatio * height;
  left = defaultPosition(1) + (col - 1) * (width + horizontalGap);
  bottom = defaultPosition(2) + (m - row) * (height + verticalGap);
  position = [left, bottom, left + width, bottom + height];
end
%=============================================================================
function tf = intersectionBetweenRectangles(rectangle1, rectangle2)
  right = min(rectangle1(1) + rectangle1(3), rectangle2(1) + rectangle2(3));
  top = min(rectangle1(2) + rectangle1(4), rectangle2(2) + rectangle2(4));
  left = max(rectangle1(1), rectangle2(1));
  bottom = max(rectangle1(2), rectangle2(2));
  inside = ((right <= left) || (top <= bottom));
  tf = false;
  if ~inside
    tf = (right - left) * (top - bottom) > .01;
  end
end
%=============================================================================
