%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function A = vander(V)
  % https://en.wikipedia.org/wiki/Vandermonde_matrix
  narginchk(1, 1);
  nargoutchk(0, 1);
  
  V = V(:);
  N = length(V);
  if N > 0
    A = zeros(length(V), N, class(V));
    D = 1;
    V = V(:);
    for i = N:-1:1
      A(:, i) = D;
      D = D .* V;
    end
  else
    A = reshape(V, N, N);
  end
end
%=============================================================================
