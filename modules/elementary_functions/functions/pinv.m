%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = pinv(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  A = varargin{1};
  if (isempty(A))
    y = A;
    return;
  end
  [U, S, V] = svd(A, 'econ');
  s = diag(S);
  if nargin < 2 
    tol = max(size(A)) * eps(norm(s, inf));
  else
    tol = varargin{2};
  end
  r1 = sum(s > tol) + 1;
  V(:, r1:end) = [];
  U(:, r1:end) = [];
  s(r1:end) = [];
  s = 1 ./ s(:);
  y = (V .* s.') * U';
end
%=============================================================================
