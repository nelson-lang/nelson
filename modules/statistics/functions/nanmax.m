%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, idx] = nanmax(x, y2, dim)
  narginchk(1, 3);
  nargoutchk(0, 2);
  if nargin == 1
    [y, idx] = max(x, [], 'omitnan');
  elseif nargin == 2
    y = max(x, y2, 'omitnan');
    idx = [];
  elseif isnumeric(dim) && ~isscalar(dim)
    % Vector of dimensions: reduce along each one (max is associative).
    y = x;
    for d = unique(double(dim(:).'))
      y = max(y, [], d, 'omitnan');
    end
    idx = [];
  else
    [y, idx] = max(x, y2, dim, 'omitnan');
  end
end
%=============================================================================
