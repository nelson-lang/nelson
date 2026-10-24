%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function dy = odeVectorizedPatternRhs(t, y)
  global ODE_VECTORIZED_PATTERN_COLUMNS;
  if isempty(ODE_VECTORIZED_PATTERN_COLUMNS)
    ODE_VECTORIZED_PATTERN_COLUMNS = [];
  end
  ODE_VECTORIZED_PATTERN_COLUMNS(end + 1) = size(y, 2);
  dy = zeros(size(y));
  dy(1, :) = -100 * y(1, :);
  dy(2, :) = -100 * y(2, :);
end
%=============================================================================
