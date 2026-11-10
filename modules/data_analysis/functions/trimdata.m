%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function B = trimdata(A, m, varargin)
  % shortens A to length m along the operating dimensions
  narginchk(2, inf);
  [lengths, dims, opts] = padTrimOptions(A, m, false, varargin);
  B = A;
  for k = 1:numel(dims)
    d = dims(k);
    n = size(B, d);
    target = lengths(k);
    if target >= n
      continue
    end
    removed = n - target;
    switch opts.Side
      case 'trailing'
        first = 1;
      case 'leading'
        first = removed + 1;
      otherwise
        first = floor(removed / 2) + 1;
    end
    subs = repmat({':'}, 1, max(ndims(B), d));
    subs{d} = first:first + target - 1;
    B = B(subs{:});
  end
end
%=============================================================================
