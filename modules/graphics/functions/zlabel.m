%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = zlabel(varargin)
  nargoutchk(0, 1);
  if (nargin < 1)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  inputArguments = convertStringToCharArgs(varargin);
  if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  % Label stays visible independently of the axes 'Visible' state.
  visibility = 'on';
  h = ax.ZLabel;
  if ~(isscalar(h) && isgraphics(h, 'text'))
    h = __text__('Parent', ax, 'AutoParent', 'off');
  end
  if isEmptyTextString(h.String)
    % Rotation and alignment are owned by the axes and re-derived from the View
    % (auto RotationMode semantics), so they update when the view changes.
    set(h, ...
      'Parent', ax, ...
      'AffectAutoLimits', 'off', ...
      'Visible', visibility);
  end
  ax.ZLabel = h;
  removeChild(ax, h);
  hasPosition = hasPropertyArgument(inputArguments(2:end), 'Position');
  hasUnits = hasPropertyArgument(inputArguments(2:end), 'Units');
  set(h, 'String', inputArguments{1}, inputArguments{2:end});
  if ~hasPosition && ~hasUnits && ~is2D(ax)
    set(h, 'Units', 'data', 'Position', defaultZLabelPosition(ax));
  end
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function position = defaultZLabelPosition(ax)
  xLimits = ax.XLim;
  yLimits = ax.YLim;
  zLimits = ax.ZLim;
  xSpan = axisSpan(xLimits);
  ySpan = axisSpan(yLimits);
  zSpan = axisSpan(zLimits);
  position = [xLimits(1) - 0.10625672002674945 * xSpan, ...
    yLimits(2) + 0.0815339091886429 * ySpan, ...
    mean(zLimits) + 3.48774332682349e-7 * zSpan];
end
%=============================================================================
function span = axisSpan(limits)
  span = limits(2) - limits(1);
  if ~(isfinite(span) && span > 0)
    span = 1;
  end
end
%=============================================================================
function tf = hasPropertyArgument(propertyArguments, name)
  tf = false;
  for k = 1:2:numel(propertyArguments) - 1
    if ischar(propertyArguments{k}) && strcmpi(propertyArguments{k}, name)
      tf = true;
      return
    end
  end
end
%=============================================================================
function removeChild(parent, h)
  if ~isgraphics(h)
    return;
  end
  children = allchild(parent);
  if ~isempty(children)
    children(children == h) = [];
    parent.Children = children;
  end
end
%=============================================================================
function tf = isEmptyTextString(value)
  tf = length(value) == 0 || strcmp(value, '');
end
%=============================================================================
