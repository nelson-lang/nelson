%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = clim(varargin)
  varargin = convertStringToCharArgs(varargin);
  if ((nargin > 0) && isscalar(varargin{1}) && isgraphics(varargin{1}))
    handle = varargin{1};
    varargin(1) = [];
    nargin = nargin - 1;
  else
    handle = gca();
  end
  if (nargin == 0)
    varargout{1} = handle.CLim;
    return;
  end
  if (ischar(varargin{1}))
    if (strcmp(varargin{1}, 'mode'))
      varargout{1} = handle.CLimMode;
      varargout = {};
      return;
    end
    if (strcmp(varargin{1}, 'auto'))
      handle.CLimMode = 'auto';
      varargout = {};
      return;
    end
    if (strcmp(varargin{1}, 'manual'))
      handle.CLimMode = 'manual';
      varargout = {};
      return;
    end
    error('Nelson:clim:invalidArgument', '"%s" or "%s" argument expected.', 'manual', 'auto');

  elseif (isnumeric(varargin{1}) && numel(varargin{1}) == 2)
    value = varargin{1};
    if ~isequal(handle.CLim, value)
      handle.CLim = value;
    end
  else
    error('Nelson:clim:errorSettingProperty', 'Error setting property');
  end
  varargout = {};
end
%=============================================================================
