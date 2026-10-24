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
  if nRhs < 2
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  end
  if nRhs > 3
    error('Nelson:tooManyInputs', 'Too many input arguments.');
  end
  X = varargin{1};
  N = varargin{2};
  sz = size(X);
  nd = ndims(X);
  if (islogical(N) || ~isnumeric(N) || ~isreal(N))
    error('Nelson:circshift:allValuesNIntegerValues3', 'All values of N must be integer values.');
  end
  if (nRhs == 3)
    DIM = varargin{3};
    if ~isscalar(N)
      error('Nelson:circshift:nAScalarIfDIM2', 'N must be a scalar if DIM is specified.');
    end
    if (~(isnumeric(DIM) || islogical(DIM)) || ~isscalar(DIM) || ~isfinite(DIM) || DIM < 1 || DIM ~= fix(DIM))
      error('Nelson:circshift:dimensionPositiveInteger', 'DIM must be a positive integer scalar.');
    end
    N = [zeros(1, DIM - 1), N];
  end
  if ~all(isfinite(N))
    error('Nelson:circshift:allValuesNIntegerValues3', 'All values of N must be integer values.');
  end
  if (nRhs == 2 && isscalar(N))
    % scalar N applies to the first non-singleton dimension
    DIM = find(sz ~= 1, 1);
    if isempty(DIM)
      DIM = 1;
    end
    N = [zeros(1, DIM - 1), N];
  end
  if ~isvector(N)
    error('Nelson:validators:mustBeVector', '%s must be a vector.', 'N');
  elseif (any(N ~= fix(N)))
    error('Nelson:circshift:allValuesNIntegerValues3', 'All values of N must be integer values.');
  end
  % a shift along a dimension beyond ndims(X) (its extent is 1) does nothing
  N = N(1:min(length(N), nd));
  IDX = repmat({':'}, 1, nd);
  if ~isempty(X)
    for i = 1:length(N)
      D = sz(i);
      B = N(i);
      if (B < 0)
        B = rem(abs(B), D);
        IDX{i} = [B + 1:D, 1:B];
      else
        if (B > 0)
          B = rem(B, D);
          IDX{i} = [D - B + 1:D, 1:D - B];
        end
      end
    end
  end
  % an empty array is indexed too: its result holds no imaginary part
  Y = X(IDX{:});
end
%=============================================================================
