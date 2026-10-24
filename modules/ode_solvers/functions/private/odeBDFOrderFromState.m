%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Current BDF order for the ode15s kernel, derived deterministically from
% the accumulated solution history so both the step kernel and the step
% size controller agree on it. BDF-k needs k history points plus one more
% for the same-order predictor, hence history length minus one, capped by
% MaxOrder and the stability limit 5.
function k = odeBDFOrderFromState(problem, solverState)
  maximumOrder = 5;
  if ~isempty(problem.options.MaxOrder)
    maximumOrder = min(maximumOrder, problem.options.MaxOrder);
  end
  available = 1;
  if isfield(solverState, 'historyValid') && solverState.historyValid ...
      && isfield(solverState, 't')
    available = length(solverState.t) - 1;
  end
  k = max(1, min(available, maximumOrder));
end
%=============================================================================
