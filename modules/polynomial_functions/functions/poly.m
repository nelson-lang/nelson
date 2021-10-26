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
function p = poly(r)
  narginchk(1, 1);
  nargoutchk(0, 1);
  [m, n] = size(r);
  if m == n
     e = eig(r);
  elseif ((m==1) || (n==1))
     e = r;
  else
     error(_('Argument must be a vector or a square matrix.'), 'Nelson:poly:InputSize')
  end
  e = e( isfinite(e) );
  n = length(e);
  p = [1 zeros(1, n, class(r))];
  for j=1:n
    p(2:(j+1)) = p(2:(j+1)) - e(j).*p(1:j);
  end
  if isequal(sort(e(imag(e) > 0)),sort(conj(e(imag(e) < 0))))
    p = real(p);
  end
end
%=============================================================================