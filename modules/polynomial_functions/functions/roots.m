%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = roots(c)
  narginchk(1, 1);
  nargoutchk(0, 1);
  
  if isempty(c)
    r = c;
    return 
  end
  
  if ~isvector(c)
    error('Nelson:roots:NonVectorInput', _('Input must be a vector.'))
  end
  
  if ~all(isfinite(c))
    error('Nelson:roots:NonFiniteInput', _('Input to roots function must not contain NaN or Inf.'));
  end
  
  
  c = c(:).';
  r = zeros(0, 1, class(c));  
  
  inz = find(c);
  if isempty(inz)
    return
  end
  
  nnz = length(inz);
  c = c(inz(1):inz(nnz));
  n = size(c, 2);
  r = zeros(n-inz(nnz), 1, class(c));  
  d = c(2:end)./c(1);
  while any(isinf(d))
    c = c(2:end);
    d = c(2:end)./c(1);
  end
  n = length(c);
  if n > 1
    A = diag(ones(1, n - 2, class(c)), -1);
    A(1, :) = -d;
    r = [r; eig(A)];
  end
end
%=============================================================================
