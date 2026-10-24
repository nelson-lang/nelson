%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% After a rejected step the variable-order BDF kernel drops the oldest
% history point, lowering the working order by one (down to BDF2). A sharp
% solution feature that keeps rejecting high-order steps is then attacked
% with a more robust low-order formula, and the order rebuilds naturally as
% steps are accepted again.
function solverState = odeReduceOrderOnFailure(problem, solverState)
  if ~strcmp(problem.solver, 'ode15s')
    return
  end
  if ~isfield(solverState, 'consecutiveRejections')
    solverState.consecutiveRejections = 0;
  end
  solverState.consecutiveRejections = solverState.consecutiveRejections + 1;
  % An isolated rejection is normal step-size calibration; only a repeated
  % failure at the same location indicates the high-order formula is
  % struggling (sharp feature) and warrants dropping an order.
  if solverState.consecutiveRejections < 2
    return
  end
  if ~isfield(solverState, 'historyValid') || ~solverState.historyValid
    return
  end
  if ~isfield(solverState, 't') || length(solverState.t) <= 3
    return
  end
  solverState.t = solverState.t(2:end);
  solverState.y = solverState.y(:, 2:end);
  solverState.slope = solverState.slope(:, 2:end);
end
%=============================================================================
