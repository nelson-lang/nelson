%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = theme(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if (nargin == 1)
    target = gcf();
    value = varargin{1};
  else
    target = varargin{1};
    value = varargin{2};
    if (~isscalar(target) || ~isgraphics(target))
      error('Nelson:theme:invalidFirstArgument', 'First argument must be a graphics object handle.');
    end
  end
  if (~isgraphics(target, 'figure'))
    target = ancestor(target, 'figure');
  end
  if (isempty(target) || ~isgraphics(target, 'figure'))
    error('Nelson:theme:figureIsRequiredTo', 'A figure is required to apply a theme.');
  end
  name = themeName(value);
  set(target, 'Theme', name);
  if (nargout == 1)
    varargout{1} = get(target, 'Theme');
  end
end
%=============================================================================
function name = themeName(value)
  if (ischar(value) || isStringScalar(value))
    name = convertStringsToChars(value);
  elseif (isstruct(value) && isfield(value, 'BaseColorStyle'))
    name = value.BaseColorStyle;
  else
    error('Nelson:figure:invalidTheme', 'Specify "light" or "dark".');
  end
  if (~(strcmpi(name, 'light') || strcmpi(name, 'dark')))
    error('Nelson:figure:invalidTheme', 'Specify "light" or "dark".');
  end
  name = lower(name);
end
%=============================================================================
