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
function p = primes(n)
  narginchk(1, 1)
  nargoutchk(1, 1)
  
  if ~isscalar(n)
    error(_('Wrong size for argument #1: scalar expected.'));
  end
  if ~isfinite(n)
    error(_('Wrong value for argument #1: finite value expected.'));
  end
  
  if ~isreal(n)
    error(_('Wrong value for argument #1: real value expected.'));
  end
  
  if n < 2
    p = zeros(1, 0, class(n));
    return
  end
  N = floor(double(n));
  p = [1:2:N];
  q = length(p);
  p(1) = 2;
  for k = 3:2:sqrt(N)
    if p((k+1)/2)
      p([((k*k+1)/2):k:q]) = 0;
    end
  end
  p = p(find(p >= 0));
  p = p(find(p ~= 0));
  p = cast(p, class(n));
end
%=============================================================================
