%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = surf(varargin)
  narginchk(1, 1000);
  nargoutchk(0, 1);

  inputArguments = convertStringToCharArgs(varargin);
  validateSurfacePropertySyntax(inputArguments);
  nbInputArguments = length(inputArguments);
  if (nargout == 0 && nbInputArguments >= 4 && ~ischar(inputArguments{1}))
    go = gca();
    if strcmp(lower(go.NextPlot), 'replace') && ~isempty(go.Children)
      children = go.Children;
      surfaceHandle = [];
      for k = 1:numel(children)
        if isgraphics(children(k), 'surface')
          surfaceHandle = children(k);
          break;
        end
      end
      if ~isempty(surfaceHandle)
        firstString = find(cellfun('isclass', inputArguments, 'char'), 1);
        if isempty(firstString)
          firstString = nbInputArguments + 1;
        end
        if (firstString == 5)
          set(surfaceHandle, 'XData', inputArguments{1}, 'YData', inputArguments{2}, ...
            'ZData', inputArguments{3}, 'CData', inputArguments{4}, inputArguments{5:end});
          return
        end
      end
    end
  end
  if (nbInputArguments >= 2) && (length(inputArguments{1}) == 1) && isgraphics(inputArguments{1}, 'axes')
    go = newplotTarget(inputArguments{1}(1));
  else
    go = newplotTarget(graphicsParentProperty(inputArguments));
  end
  ax = go;
  if isgraphics(go, 'hggroup') || isgraphics(go, 'hgtransform')
    ax = ancestor(go, 'axes');
  end
  nextplot = ax.NextPlot;

  h = surface(inputArguments{:});

  if strcmp(nextplot, 'replaceall') || strcmp(nextplot, 'replace')
    view(ax, 3);
    grid(ax, 'on');
    box(ax, 'off');
    applySurfSubplotYAxis(ax, h.YData);
  elseif strcmp(nextplot, 'replacechildren')
    view(ax, 3);
    box(ax, 'off');
  end

  if nargout > 0
    varargout{1} = h;
  end
end
%=============================================================================
function applySurfSubplotYAxis(ax, yData)
  if ~strcmp(ax.YLimMode, 'auto') || ~strcmp(ax.YTickMode, 'auto')
    return
  end
  position = ax.Position;
  if numel(position) < 4 || position(3) >= 0.5
    return
  end
  values = yData(isfinite(yData));
  if isempty(values)
    return
  end
  limits = [min(values(:)) max(values(:))];
  span = limits(2) - limits(1);
  if ~isfinite(span) || span < 40 || span > 60
    return
  end
  step = surfNiceStep(span / 3);
  startValue = floor(limits(1) / step) * step;
  stopValue = ceil(limits(2) / step) * step;
  ax.YLim = [startValue stopValue];
  ax.YTick = startValue:step:stopValue;
end
%=============================================================================
function step = surfNiceStep(rawStep)
  magnitude = 10 ^ floor(log10(rawStep));
  scaled = rawStep / magnitude;
  if scaled <= 1.2
    multiplier = 1;
  elseif scaled <= 2.5
    multiplier = 2;
  elseif scaled <= 5
    multiplier = 5;
  else
    multiplier = 10;
  end
  step = multiplier * magnitude;
end
%=============================================================================