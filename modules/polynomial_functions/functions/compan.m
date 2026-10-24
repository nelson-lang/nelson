%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = compan(c)
  narginchk(1, 1);
  if ~isvector(c)
    error('Nelson:compan:vectorExpected', 'Input argument must be a vector.');
  end
  c = c(:).';
  n = length(c);
  if n <= 1
    A = [];
  else
    A = zeros(n - 1, n - 1, class(c));
    A(1, :) = -c(2:n) / c(1);
    if n > 2
      A(2:n - 1, 1:n - 2) = eye(n - 2, class(c));
    end
  end
end
%=============================================================================
