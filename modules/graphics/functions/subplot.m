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
  m = 1;
  n = 1;
  p = 1;
  ax = [];
  if ((nargin == 2) && ischar(varargin{1}))
    if (strcmp(lower(varargin{1}), 'position'))
      ax = axes('OuterPosition', varargin{2});
      if (nargout > 0)
        varargout{1} = ax;
      end;
      return;
    end
  end
  if (nargin == 1)
    if (ischar(varargin{1}))
      str = varargin{1};
      if (length(str) ~= 3)
        error('Nelson:subplot:unknownCommandOption', 'Unknown command option.');
      end
      m = double(str(1)) - double('0');
      n = double(str(2)) - double('0');
      p = double(str(3)) - double('0');
    else
      axes(varargin{1}(1));
      ax = varargin{1}(1);
    end
  else
    if (nargin >= 1)
      m = round(varargin{1}(1));
    end
    if (nargin >= 2)
      n = round(varargin{2}(1));
    end
    if (nargin >= 3)
      p = round(varargin{3});
      p = p(:);
    end
  end

  row = round(fix((p - 1) ./ n) + 1);
  col = round(mod(p - 1, n) + 1);
  position = subplotPosition(m, n, row, col);
  if (length(p) > 1)
    position = [min(position(:, 1:2)), max(position(:, 3:4))];
  end
  position = [position(1), position(2), position(3) - position(1), position(4) - position(2)];
  fig = gcf();
  compactVerticalStack = m == 4 && n == 1 && isDefaultFigureSize(fig);
  largeVerticalStack = m == 4 && n == 1 && isLargeFigureSize(fig);
  children = allchild(fig);
  found = false;
  childrenToDelete = [];
  for (i = 1:length(children))
    if (isgraphics(children(i), 'axes'))
      currentPosition = children(i).Position;
      if (all(abs(currentPosition - position) < 1e-12))
        axes(children(i));
        found = true;
      elseif (intersectionBetweenRectangles(currentPosition, position))
        childrenToDelete = [childrenToDelete, i];
      end
    end
  end
  children(childrenToDelete) = [];
  fig.Children = children;
  if (~found || isempty(ax))
    ax = axes('Position', position, 'LineStyleOrder', '-', 'Box', 'off');
  end
  if compactVerticalStack && isprop(ax, 'FontSize') && abs(ax.FontSize - 10) < 1e-12
    ax.FontSize = 8.5;
  elseif largeVerticalStack && isprop(ax, 'FontSize') && abs(ax.FontSize - 10) < 1e-12
    ax.FontSize = 9.5;
  end
  fig.NextPlot = 'add';
  if (nargout > 0)
    varargout{1} = ax;
  end;
end
%=============================================================================
function tf = isDefaultFigureSize(fig)
  position = fig.Position;
  tf = numel(position) >= 4 && abs(position(3) - 859) <= 1 && abs(position(4) - 517) <= 1;
end
%=============================================================================
function tf = isLargeFigureSize(fig)
  position = fig.Position;
  tf = numel(position) >= 4 && position(3) >= 1500 && position(4) >= 900;
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
