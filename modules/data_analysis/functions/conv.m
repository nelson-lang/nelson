%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function z = conv(a, b, shape)
  if nargin < 3
    shape = 'full';
  end    
  a_is_col = size(a, 1) > size(a, 2);
  b_is_col = size(b, 1) > size(b, 2);
  if (a_is_col || b_is_col)
    z = conv2(a(:), b(:), shape);
  else
    z = conv2(a(:).', b(:).', shape);
  end
  if (numel(a) <= numel(b))
    maxdims = min(find(size(b) == max(size(b))));
  else
    maxdims = min(find(size(a) == max(size(a))));
  end
  if (maxdims > 1)
    z = reshape(z, [ones(1, maxdims - 1), numel(z)]);
  end
end
%=============================================================================
