%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function f = factor(n)
  narginchk(1, 1);
  nargoutchk(0, 1);

  if ~isscalar(n)
    error('Nelson:scalarExpected', 'Wrong size for argument #%d: scalar expected.', 1);
  end
  if ~isfinite(n)
    error('Nelson:primes:mustBeFinite', 'Wrong value for argument #%d: finite value expected.', 1);
  end
  if ~isreal(n)
    error('Nelson:primes:mustBeReal', 'Wrong value for argument #%d: real value expected.', 1);
  end

  if (n < 0) || (floor(n) ~= n)
    error('Nelson:factor:mustBeNonnegativeInteger', 'Wrong value for argument #%d: a nonnegative integer expected.', 1);
  end

  if (n > 2 ^ 32)
    error('Nelson:factor:valueTooLarge', 'Wrong value for argument #%d: maximum value of n allowed is 2^32.', 1);
  end

  if n < 4
    f = n;
    return;
  end

  outputClass = class(n);
  remainingValue = double(n);
  candidatePrimes = primes(sqrt(remainingValue));
  f = [];
  while remainingValue > 1
    divisorMask = rem(remainingValue, candidatePrimes) == 0;
    if ~any(divisorMask)
      f = [f, remainingValue];
      break;
    end
    divisors = candidatePrimes(divisorMask);
    f = [f, divisors];
    remainingValue = remainingValue / prod(divisors);
    candidatePrimes = divisors;
  end
  f = sort(f);
  f = cast(f, outputClass);
end
%=============================================================================
