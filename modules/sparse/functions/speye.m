%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sp = speye(varargin)
  if (nargin == 0)
    m = 1;
    n = 1;
  elseif (nargin == 1)
    arg = varargin{1};
    if isscalar(arg)
      m = arg;
      n = m;
    elseif isvector(arg)
      len = length(arg);
      if (len == 2)
        m = arg(1);
        n = arg(2);
      else
        error('Nelson:speye:InvalidInputFormat', _('Please use speye(n), speye(m, n) or speye([m, n]).'));
      end
    else
      error('Nelson:speye:InvalidInputFormat', _('Please use speye(n), speye(m, n) or speye([m, n]).'));
    end
  elseif (nargin == 2)
    m = varargin{1};
    n = varargin{2};
    if ~isscalar(m) || ~isscalar(n)
      error('Nelson:speye:InvalidInputFormat', _('Please use speye(n), speye(m, n) or speye([m, n]).'));
    end
  else
    error(_('Wrong number of output arguments.'));
  end
  if (m < 0)
    m = 0;
  end
  if (n < 0)
    n = 0;
  end
  mn = min([m, n]);
  idx = 1:mn;
  sp = sparse(idx, idx, ones(mn, 1), m, n);
end
%=============================================================================
