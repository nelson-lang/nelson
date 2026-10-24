%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = rank(A, tol)
  if ~ismatrix(A)
    error('Nelson:rank:inputMustBe2D', 'Input must be 2-dimensional.');
  end
  if ~all(isfinite(A(:)))
    error('Nelson:rank:nonFiniteInput', 'Input matrix must not contain NaN or Inf values.');
  end
  if nargin == 2 && (~isnumeric(tol) || ~isrow(tol) || ~isreal(tol))
    error('Nelson:rank:invalidTolerance', 'Second input must be a real numeric tolerance.');
  end
  S = svd(A);
  if (nargin == 1)
    maxS = max(S);
    if isempty(maxS)
      epsMax = eps;
    else
      epsMax = eps(maxS);
    end
    tol = max(size(A)) * epsMax;
  end
  R = sum(S > tol);
end
%=============================================================================
