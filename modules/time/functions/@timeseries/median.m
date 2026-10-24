%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = median(obj, varargin)
  data = sort(obj.Data, varargin{:});
  n = size(data, 1);
  if n == 0
    value = [];
  elseif rem(n, 2) == 1
    value = data((n + 1) / 2, :);
  else
    value = (data(n / 2, :) + data(n / 2 + 1, :)) ./ 2;
  end
end
%=============================================================================
