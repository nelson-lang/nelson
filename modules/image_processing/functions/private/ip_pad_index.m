%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function idx = ip_pad_index(idx, n, method)
  if strcmp(method, 'replicate')
    idx = min(max(idx, 1), n);
  elseif strcmp(method, 'circular')
    idx = mod(idx - 1, n) + 1;
  elseif strcmp(method, 'symmetric')
    if n == 1
      idx = ones(size(idx));
    else
      p = 2 * n;
      idx = mod(idx - 1, p) + 1;
      idx = idx .* (idx <= n) + (p - idx + 1) .* (idx > n);
    end
  else
    error('Nelson:image_processing:unsupportedPaddingMethod', 'Unsupported "%s" option.', 'padding method');
  end
end
%=============================================================================
