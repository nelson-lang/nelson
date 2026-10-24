%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0014,NLS0015,NLS0026>
function y = cond(A, p)
  narginchk(1, 2);
  if (nargin < 2)
    normType = 2;
  else
    normType = p;
  end

  if ~isequal(normType, 2)
    sizeMismatch = ismatrix(A) && size(A, 1) ~= size(A, 2);
    if sizeMismatch
      error('Nelson:linear_algebra:squareMatrixExpected', '%s expected.', 'Square matrix');
    end
  end

  if (isempty(A))
    y = 0;
    return;
  end
  if (normType ~= 2)
    y = norm(A, normType) * norm(inv(A), normType);
  else
    if (isscalar(A) && (A == 0))
      y = inf;
    else
      singularValues = svd(A);
      if any(singularValues == 0)
        y = cast(Inf, class(A));
      else
        y = max(singularValues) ./ min(singularValues);
        if isempty(y)
          y = zeros(class(A));
        end
      end
    end
  end
end
%=============================================================================
