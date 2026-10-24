%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function P = perms(v)
  narginchk(1, 1);
  nargoutchk(0, 1);
  v = v(:).';
  n = numel(v);
  if n <= 1
    P = v;
    return;
  end
  P = v(local_perms_index(n));
end
%=============================================================================
function P = local_perms_index(n)
  % permutations of 1:n as row indices, in reverse-lexicographic order
  if n == 1
    P = 1;
    return;
  end
  Q = local_perms_index(n - 1);
  m = size(Q, 1);
  P = zeros(n * m, n);
  for first = n:-1:1
    block = (n - first) * m + (1:m);
    rest = [1:first - 1, first + 1:n];
    P(block, 1) = first;
    P(block, 2:n) = rest(Q);
  end
end
%=============================================================================
