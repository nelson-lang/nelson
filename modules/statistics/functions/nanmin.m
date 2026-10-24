%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, idx] = nanmin(x, y2, dim)
  narginchk(1, 3);
  nargoutchk(0, 2);
  if nargin == 1
    [y, idx] = min(x, [], 'omitnan');
  elseif nargin == 2
    y = min(x, y2, 'omitnan');
    idx = [];
  elseif isnumeric(dim) && ~isscalar(dim)
    % Vector of dimensions: reduce along each one (min is associative).
    y = x;
    for d = unique(double(dim(:).'))
      y = min(y, [], d, 'omitnan');
    end
    idx = [];
  else
    [y, idx] = min(x, y2, dim, 'omitnan');
  end
end
%=============================================================================
