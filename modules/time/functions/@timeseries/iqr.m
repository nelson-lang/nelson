%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = iqr(obj, varargin)
  data = sort(obj.Data, varargin{:});
  value = localPercentile(data, 75) - localPercentile(data, 25);
end
%=============================================================================
function value = localPercentile(data, pct)
  if isempty(data)
    value = [];
    return
  end
  n = size(data, 1);
  pos = 1 + (n - 1) .* pct ./ 100;
  lo = floor(pos);
  hi = ceil(pos);
  w = pos - lo;
  value = data(lo, :) .* (1 - w) + data(hi, :) .* w;
end
%=============================================================================
