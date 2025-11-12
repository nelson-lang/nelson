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
  if (length(inputArguments) > 0 && isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  
  if (ischar(inputArguments{1}))
    handle = __line__('Parent', ax, inputArguments{:});
  elseif (numel(inputArguments) == 2)
    x = inputArguments{1};
    y = inputArguments{2};
    handle = __line__('Parent', ax, 'XData', x, 'YData', y);
  elseif (numel(inputArguments) >= 3)
    x = inputArguments{1};
    y = inputArguments{2};
    inputArguments = inputArguments(3:end);
    if isnumeric(inputArguments{1})
      z = inputArguments{1};
      inputArguments = inputArguments(2:end);
    else
      z = ones(0, 1); 
    end
    if isempty(inputArguments)
      handle = __line__('Parent', ax, 'XData', x, 'YData', y, 'ZData', z);
    else
      handle = __line__('Parent', ax, 'XData', x, 'YData', y, 'ZData', z, inputArguments{:});
    end
  else
    error(_('Wrong input arguments.'));
  end
  if (nargout > 0)
    varargout{1} = handle;
  end
  %=============================================================================
