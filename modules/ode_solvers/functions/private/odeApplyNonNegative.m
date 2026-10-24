%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = odeApplyNonNegative(problem, y)
  idx = problem.options.NonNegative;
  if ~isempty(idx)
    idx = idx(:);
    y(idx) = max(y(idx), 0);
  end
end
%=============================================================================
