%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
  p = primes(cast(sqrt(double(n)), class(n)));
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
end
%=============================================================================
