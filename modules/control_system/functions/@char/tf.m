%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = tf(varargin)
  narginchk(1, 2);
  sys = {};
  if (nargin == 1)
    param1 = varargin{1};
    if ischar(param1)
      isInvalid = true;
      if strcmp(param1, 's')
        sys = tf([1 0], [0 1]);
        sys.Variable = 's';
        isInvalid = false;
      end
      if strcmp(param1, 'z')
        sys = tf([1 0], [0 1]);
        sys.Variable = 'z';
        isInvalid = false;
      end
      if strcmp(param1, 'p')
        sys = tf([1 0], [0 1]);
        sys.Variable = 'p';
        isInvalid = false;
      end
      if strcmp(param1, 'q')
        sys = tf([1 0], [0 1]);
        sys.Variable = 'q';
        isInvalid = false;
      end
      if isInvalid
        error(_('Invalid syntax: ''s'', ''z'', ''p'', ''q'' expected.'));
      end
    else
      error(_('Invalid syntax: ''s'', ''z'', ''p'', ''q'' expected.'));
    end
  else
    param1 = varargin{1};
    Ts = varargin{2};
    if ischar(param1) && strcmp(param1, 'z')
      sys = tf([1 0], [0 1]);
      sys.Variable = 'z';
    else
      error(_('Invalid syntax: ''s''expected.'));
    end
    if isempty(Ts) || (isscalar(Ts) && (isequal(Ts, -1) || Ts > 0))
      sys.Ts = Ts;
    else
      error(_('Ts property must be 0, a positive scalar, or  -1.'));
    end
  end
end
%=============================================================================
