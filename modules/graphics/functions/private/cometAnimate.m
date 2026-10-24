%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function cometAnimate(ax, x, y, z, p)
  is3D = ~isempty(z);
  if isempty(ax)
    ax = newplot();
  else
    ax = newplot(ax);
  end

  if ~strcmp(ax.NextPlot, 'add')
    [minX, maxX] = finiteMinMax(x);
    [minY, maxY] = finiteMinMax(y);
    if is3D
      [minZ, maxZ] = finiteMinMax(z);
      axis(ax, [minX maxX minY maxY minZ maxZ]);
      view(ax, 3);
    else
      axis(ax, [minX maxX minY maxY]);
    end
  end

  totalLength = numel(x);
  bodyLength = round(p * totalLength);
  bodyMaximum = max(1, bodyLength);
  colors = cometColors(ax);
  updateStep = cometUpdateStep(totalLength);
  if is3D
    head = line('Parent', ax, 'Marker', 'o', 'Color', colors(1, :), ...
      'MarkerEdgeColor', colors(1, :), 'XData', x(1), 'YData', y(1), ...
      'ZData', z(1), 'Tag', 'head');
  else
    head = line('Parent', ax, 'Marker', 'o', 'LineStyle', 'none', ...
      'Color', colors(1, :), 'MarkerEdgeColor', colors(1, :), ...
      'XData', x(1), 'YData', y(1), 'Tag', 'head');
  end
  body = animatedline(ax, 'MaximumNumPoints', bodyMaximum, 'Color', colors(2, :), ...
    'Tag', 'body');
  tail = animatedline(ax, 'LineStyle', '-', 'MaximumNumPoints', 1 + totalLength, ...
    'Color', colors(3, :), 'Tag', 'tail');
  if isequal(body.Color, tail.Color) && isequal(body.LineStyle, tail.LineStyle)
    body.LineStyle = '--';
  end

  for first = 1:updateStep:bodyLength
    if ~(isgraphics(head) && isgraphics(body))
      return
    end
    last = min(bodyLength, first + updateStep - 1);
    updateHead(head, x(last), y(last), z, last);
    addCometPoints(body, x, y, z, first:last);
    drawnow();
  end
  drawnow();

  addCometPoints(tail, x, y, z, 1);

  for first = bodyLength + 1:updateStep:totalLength
    if ~(isgraphics(head) && isgraphics(body) && isgraphics(tail))
      return
    end
    last = min(totalLength, first + updateStep - 1);
    updateHead(head, x(last), y(last), z, last);
    addCometPoints(body, x, y, z, first:last);
    tailFirst = first - bodyLength + 1;
    tailLast = min(totalLength, last - bodyLength + 1);
    addCometPoints(tail, x, y, z, tailFirst:tailLast);
    drawnow();
  end
  drawnow();

  for first = totalLength - bodyLength + 2:updateStep:totalLength
    if ~isgraphics(tail)
      return
    end
    last = min(totalLength, first + updateStep - 1);
    addCometPoints(tail, x, y, z, first:last);
    drawnow();
  end
  drawnow();
end
%=============================================================================
function updateHead(head, x, y, z, index)
  head.XData = x;
  head.YData = y;
  if ~isempty(z)
    head.ZData = z(index);
  end
end
%=============================================================================
function addCometPoints(object, x, y, z, index)
  if isempty(z)
    addpoints(object, x(index), y(index));
  else
    addpoints(object, x(index), y(index), z(index));
  end
end
%=============================================================================
function colors = cometColors(ax)
  colors = zeros(3, 3);
  for k = 1:3
    colors(k, :) = getColorAndUpdateIndex(ax);
  end
end
%=============================================================================
function updateStep = cometUpdateStep(totalLength)
  if totalLength < 2000
    updateStep = 1;
  else
    updateStep = max(1, ceil(totalLength / 200));
  end
end
%=============================================================================
function [minValue, maxValue] = finiteMinMax(values)
  finiteValues = values(isfinite(values));
  if isempty(finiteValues)
    minValue = 0;
    maxValue = 1;
    return
  end
  minValue = min(finiteValues);
  maxValue = max(finiteValues);
  if minValue == maxValue
    minValue = maxValue - 1;
    maxValue = maxValue + 1;
  end
end
%=============================================================================
