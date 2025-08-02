%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = circshift(varargin)
  nRhs = nargin;
  if (nRhs < 2 || nRhs > 3)
    error(_('Wrong number of input arguments.'));
  end
  X = varargin{1};
  N = varargin{2};
  sz = size(X);
  nd = ndims(X);
  if (nRhs == 3)
    DIM = varargin{3};
    if ~isscalar(N)
      error (_('N must be a scalar if DIM is specified.'));
    end
    N = [zeros(1, DIM - 1), N];
  end
  if ~isfinite(N)
    error (_('All values of N must be integer values.'));
  end
  if ~isempty(X)
    if (~isvector(N) || length(N) > nd)
      error (_('N must be a vector.'));
    elseif (any (N ~= fix(N)))
      error (_('All values of N must be integer values.'));
    end
    IDX = repmat ({':'}, 1, nd);
    for i = 1:length(N)
      D = sz(i);
      B = N(i);
      if (B < 0)
        B = rem(abs(B), D);
        IDX{i} = [B + 1 : D, 1 : B];
      else
        if (B > 0)
          B = rem(B, D);
          IDX{i} = [D - B + 1 : D, 1 : D - B];
        end
      end
    end
    Y = X(IDX{:});
  else
    Y = X;
  end
end
%=============================================================================
