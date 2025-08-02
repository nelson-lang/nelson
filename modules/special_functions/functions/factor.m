%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function f = factor(n)
  narginchk(1, 1)
  nargoutchk(0, 1)
  
  if ~isscalar(n)
    error(_('Wrong size for argument #1: scalar expected.'));
  end
  if ~isfinite(n)
    error(_('Wrong value for argument #1: finite value expected.'));
  end
  if ~isreal(n)
    error(_('Wrong value for argument #1: real value expected.'));
  end
  
  if (n < 0) || (floor(n) ~= n)
    error(_('Wrong value for input argument #1: a nonnegative integer expected.'));
  end
  
  if (n > 2^32)
    error(_('Wrong value for input argument #1: maximum value of n allowed is 2^32.'));
  end
  
  if (n < 4)
    f = n;
    return;
  else
    f = [];
  end
  cl = class(n);
  n = double(n);
  p = primes(sqrt(double(n)));
  while n>1
    d = find(rem(n, p) == 0);
    if isempty(d)
      f = [f, n];
      break; 
    end
    p = p(d);
    f = [f, p];
    n = n / prod(p);
  end
  f = sort(f);
  f = cast(f, cl);
end
%=============================================================================
