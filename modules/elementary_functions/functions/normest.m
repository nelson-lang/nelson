%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = normest(varargin)
  nargoutchk(0, 2);
  narginchk(1, 2);

  A = varargin{1};

  if ((~isnumeric(A) && ~islogical(A)) || (ndims(A) ~= 2))
    error('Nelson:elementary_functions:numeric2DMatrixExpected', 'An numeric 2-D matrix expected.');
  end
  if (islogical(A))
    A = double(A);
  end

  if nargin == 2
    tolerance = varargin{2};
    if (~isempty(tolerance) && ~(isscalar(tolerance) && isnumeric(tolerance)))
      error('Nelson:elementary_functions:toleranceNumericScalar', '%s must be a numeric scalar.', 'tolerance');
    end
    tolerance = real(tolerance);
    if (~isempty(tolerance) && (~isfinite(tolerance) || tolerance < 0))
      error('Nelson:elementary_functions:toleranceNonNegativeFiniteScalar', 'tolerance must be a nonnegative finite scalar.');
    end
  else
    tolerance = 1.e-6;
  end
  maxIteration = 100;
  x = full(sum(abs(A), 1))';
  if (~isreal(A))
    x = x + zeros(size(x), class(x)) * 1i;
  end
  count = 0;
  matrixNorm = norm(x);
  if (matrixNorm == 0)
    varargout{1} = matrixNorm;
    varargout{2} = count;
    return;
  end
  if (isempty(tolerance))
    varargout{1} = matrixNorm;
    varargout{2} = count;
    return;
  end
  x = x / matrixNorm;
  n0 = 0;
  while (abs(matrixNorm - n0) > (tolerance * matrixNorm))
    Ax = A * x;
    if (nnz(Ax) == 0)
      Ax = rand(size(Ax), class(Ax));
    end
    x = A' * Ax;
    normx = norm(x);
    n0 = matrixNorm;
    matrixNorm = normx / norm(Ax);
    x = x / normx;
    count = count + 1;
    if (count > maxIteration)
      warning('Nelson:normest:notconverge', 'normest did not converge for %d iterations with tolerance %g', maxIteration, tolerance);
      break;
    end
  end
  varargout{1} = matrixNorm;
  varargout{2} = count;
end
%=============================================================================
