%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function p = primes(n)
  narginchk(1, 1)
  nargoutchk(0, 1)

  if ~isscalar(n)
    error('Nelson:scalarExpected', 'Wrong size for argument #%d: scalar expected.', 1);
  end
  if ~isreal(n)
    error('Nelson:primes:mustBeReal', 'Wrong value for argument #%d: real value expected.', 1);
  end
  if n < 2
    if ischar(n)
      p = zeros(1, 0);
    else
      p = zeros(1, 0, class(n));
    end
    return
  end
  if ischar(n)
    n = double(n);
  end
  if ~isfinite(n)
    error('Nelson:primes:mustBeFinite', 'Wrong value for argument #%d: finite value expected.', 1);
  end
  N = floor(double(n));
  p = [1:2:N];
  q = length(p);
  p(1) = 2;
  for k = 3:2:sqrt(N)
    if p((k + 1) / 2)
      p([((k * k + 1) / 2):k:q]) = 0;
    end
  end
  p = p(find(p >= 0));
  p = p(find(p ~= 0));
  p = cast(p, class(n));
end
%=============================================================================
