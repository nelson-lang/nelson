%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
    error('Nelson:poly:InputSize', _('Argument must be a vector or a square matrix.'))
  end
  e = e( isfinite(e) );
  n = length(e);
  p = [1 zeros(1, n, class(r))];
  for j=1:n
    p(2:(j+1)) = p(2:(j+1)) - e(j).*p(1:j);
  end
  sortE = sort(e(imag(e) > 0));
  sortConjE = sort(conj(e(imag(e) < 0)));
  if isequal(sortE, sortConjE)
    p = real(p);
  end
end
%=============================================================================
