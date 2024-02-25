%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rotate3d(varargin)
  % rotate3d option
  % rotate3d
  % rotate3d(fig, ...)
  % rotate3d(ax, ...)
  
  narginchk(0, 2);
  if (nargin == 0)
    ax = gca();
    option = 'toggle';
    optionPosition = 1;
  else
    optionPosition = 1;
    if (isgraphics(varargin{1}, 'axes'))
      ax = varargin{1};
      option = varargin{2};
      optionPosition = 2;
    elseif (isgraphics(varargin{1}, 'figure'))
      fig = varargin{1};
      ax = fig.CurrentAxes;
      option = varargin{2};
      optionPosition = 2;
    else
      ax = gca();
      option = varargin{1};
      optionPosition = 1;
    end
  end
  if ((ischar(option) && isrow(option)) || isStringScalar(option))
    option = convertStringsToChars(option);
    mustBeMember(tolower(option), {'on', 'off', 'toggle'}, optionPosition);
    __rotate3d__(ax, option);
  else
    error(_('Wrong type for input arguments.'));
  end
end
%=============================================================================
