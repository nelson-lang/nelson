%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
