%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = zlabel(varargin)
  nargoutchk(0, 1);
  if (nargin < 1) 
    error(_('zlabel needs at least one argument.'));
  end
  inputArguments = varargin;
  if isgraphics(inputArguments{1}, 'axes')
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  h = __text__('String', inputArguments{1}, ...
  'Parent', ax, ...
  'HorizontalAlignment', 'center', ...
  'VerticalAlignment', 'middle', ...
  'AutoParent', 'off', ...
  inputArguments{2:end});
  ax.ZLabel = h;
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
