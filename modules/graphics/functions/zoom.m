%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function zoom(varargin)
  % zoom option
  % zoom
  % zoom(factor)
  % zoom(fig, ...)
  % zoom(ax, ...)
  
  narginchk(0, 2);
  optionPosition = 1;
  if (isgraphics(varargin{1}, 'axes'))
    ax = varargin{1};
    optionOrFactor = varargin{2};
    optionPosition = 2;
  elseif (isgraphics(varargin{1}, 'figure'))
    fig = varargin{1};
    ax = fig.CurrentAxes;
    optionOrFactor = varargin{2};
    optionPosition = 2;
  else
    ax = gca();
    if (nargin == 1)
      optionOrFactor = varargin{1};
    else
      optionOrFactor = 'toggle';
    end
    optionPosition = 1;
  end
  if isnumeric(optionOrFactor) && isscalar(optionOrFactor)
    factor = optionOrFactor;
    __zoom__(ax, factor);
  elseif ((ischar(optionOrFactor) && isrow(optionOrFactor)) || isStringScalar(optionOrFactor))
    option = convertStringsToChars(optionOrFactor);
    mustBeMember(tolower(option), {'on', 'off', 'reset', 'out', 'xon', 'yon', 'toggle'}, optionPosition);
    __zoom__(ax, option);
  else
    error(_('Wrong type for input arguments.'));
  end
end
%=============================================================================
