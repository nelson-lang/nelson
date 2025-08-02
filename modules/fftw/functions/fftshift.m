%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = fftshift(varargin)
  nRhs = nargin;
  if (nRhs < 1 || nRhs > 2)
    error(_('Wrong number of input arguments.'));
  end
  X = varargin{1};
  if (~(isnumeric(X) || islogical(X) || ischar(X)))
    error(_('X must be a vector or matrix.'));
  end
  if nRhs == 1
    Y = circshift(X, floor(size(X) * inv(2)));
  else
    DIM = varargin{2};
    if ((~isscalar(DIM)) || (DIM < 1) || (floor(DIM) ~= DIM))
      error(_('All values of DIM must be integer values.'));
    end
    Y = circshift(X, floor(size(X, DIM) * inv(2)), DIM);
  end
end
%=============================================================================
