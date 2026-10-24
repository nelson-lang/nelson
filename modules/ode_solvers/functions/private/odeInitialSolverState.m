%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function solverState = odeInitialSolverState(problem, t, y, slope)
  solverState = struct();
  solverState.solver = problem.solver;
  solverState.t = t;
  solverState.y = y(:);
  solverState.slope = slope(:);
  solverState.historyValid = true;
end
%=============================================================================
