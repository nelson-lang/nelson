%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function l = lcm(a, b)
  narginchk(2, 2);
  nargoutchk(0, 1);
  if islogical(a) || ischar(a)
    a = double(a);
  end
  if islogical(b) || ischar(b)
    b = double(b);
  end
  if ~isnumeric(a) || ~isnumeric(b)
    error('Nelson:lcm:nonNumericInput', 'Inputs must be numeric.');
  end
  if ~isreal(a) || ~isreal(b) || any(a(:) ~= fix(a(:))) || any(b(:) ~= fix(b(:)))
    error('Nelson:gcd:mustBeRealIntegers', 'Inputs must be real integers.');
  end
  if any(a(:) <= 0) || any(b(:) <= 0)
    error('Nelson:lcm:nonPositiveIntegers', 'Input arguments must contain positive integers.');
  end
  g = gcd(a, b);
  l = a ./ g .* b;
end
%=============================================================================
