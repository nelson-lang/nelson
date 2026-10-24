%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function value = var(obj, varargin)
  data = obj.Data;
  n = size(data, 1);
  if n <= 1
    value = zeros(1, size(data, 2));
    return
  end
  centered = data - mean(data, 1);
  value = sum(centered .* centered, 1) ./ (n - 1);
end
%=============================================================================
