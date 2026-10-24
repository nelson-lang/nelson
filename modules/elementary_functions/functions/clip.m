%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Y = clip(X, lowerBound, upperBound)
  narginchk(3, 3);
  nargoutchk(0, 1);
  if ~isnumeric(X) && ~islogical(X)
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'First input');
  end
  if ~(isnumeric(lowerBound) && isscalar(lowerBound))
    error('Nelson:elementary_functions:lowerBoundNumericScalar', '%s must be a numeric scalar.', 'Lower bound');
  end
  if ~(isnumeric(upperBound) && isscalar(upperBound))
    error('Nelson:elementary_functions:upperBoundNumericScalar', '%s must be a numeric scalar.', 'Upper bound');
  end
  if lowerBound > upperBound
    error('Nelson:elementary_functions:lowerBoundLessEqualUpper', 'Lower bound must be less than or equal to the upper bound.');
  end
  if isempty(X)
    Y = X;
    return
  end
  Y = min(max(X, lowerBound), upperBound);
  if isfloat(X)
    Y(isnan(X)) = X(isnan(X));
  end
end
%=============================================================================
