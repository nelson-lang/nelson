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
    error(_('xlabel needs at least one argument.'));
  end
  inputArguments = varargin;
  if (isscalar(inputArguments{1}) && (isgraphics(inputArguments{1}, 'axes') || isgraphics(inputArguments{1}, 'hggroup')))
    ax = inputArguments{1};
    inputArguments = inputArguments(2:end);
  else
    ax = gca();
  end
  visibility = ax.Visible;
  h = __text__('String', inputArguments{1}, ...
  'Parent', ax, ...
  'HorizontalAlignment', 'center', ...
  'VerticalAlignment', 'middle', ...
  'AutoParent', 'off', ...
  'Visible', visibility, ...
  inputArguments{2:end});
  ax.XLabel = h;
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
