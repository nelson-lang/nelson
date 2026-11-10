%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function e = __gpuArray_end__(a, k, n)
  % last index of subscript k out of n; the last subscript spans the trailing dimensions
  d = __gpuArray_size__(a);
  if n == 1
    e = prod(d);
    return
  end
  d(numel(d) + 1:n) = 1;
  if k < n
    e = d(k);
  else
    e = prod(d(k:numel(d)));
  end
end
%=============================================================================
