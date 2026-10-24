%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xlabel(varargin)
  nargoutchk(0, 1);
  if (nargin < 1)
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  inputArguments = convertStringToCharArgs(varargin);
  if (isscalar(inputArguments{1}) && isgraphics(inputArguments{1}, 'tiledlayout'))
    t = inputArguments{1};
    inputArguments = inputArguments(2:end);
    h = t.XLabel;
    set(h, 'String', inputArguments{1}, ...
      'HorizontalAlignment', 'center', ...
      'VerticalAlignment', 'top', ...
      inputArguments{2:end});
    if nargout == 1
      varargout{1} = h;
    end
    return;
  elseif (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  % Label stays visible independently of the axes 'Visible' state.
  visibility = 'on';
  h = ax.XLabel;
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
  ax.XLabel = h;
  removeChild(ax, h);
  set(h, 'String', inputArguments{1}, inputArguments{2:end});
  if nargout == 1
    varargout{1} = h;
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
