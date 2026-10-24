%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
capabilities = __ode_sundials_capabilities__();
if ~capabilities.available || ~capabilities.spgmr
  return
end
previousRuntime = getenv('NELSON_SUNDIALS_RUNTIME');
runtimeCleanup = onCleanup(@() setenv('NELSON_SUNDIALS_RUNTIME', previousRuntime));
setenv('NELSON_SUNDIALS_RUNTIME', 'ON');
n = 2001;
options = nelson.ode.options.CVODESStiff('LinearSolver', 'spgmr', ...
  'Preconditioner', 'ilu0');
problem = ode('ODEFcn', @(t, y) - y, ...
  'InitialValue', ones(n, 1), ...
  'RelativeTolerance', 1e-5, ...
  'AbsoluteTolerance', 1e-7, ...
  'Jacobian', odeJacobian(-speye(n), 'Pattern', speye(n)), ...
  'SolverOptions', options);
fprintf('bench_ode_sparse_preconditioner\n');
tic();
result = solve(problem, 0, 1e-4);
elapsed = toc();
fprintf('  spgmr+ilu0 sparse n=%d: %.6f s, setups=%d, solves=%d\n', ...
  n, elapsed, result.RawSolution.stats.preconditionerSetupCount, ...
  result.RawSolution.stats.preconditionerSolveCount);
asserts.isequal(result.RawSolution.stats.preconditioner, 'ilu0');
asserts.istrue(abs(result.Solution(1, length(result.Time)) - exp(-1e-4)) < 1e-5);
nLarge = 8001;
largeProblem = ode('ODEFcn', @(t, y) - y, ...
  'InitialValue', ones(nLarge, 1), ...
  'RelativeTolerance', 1e-5, ...
  'AbsoluteTolerance', 1e-7, ...
  'Jacobian', odeJacobian(-speye(nLarge), 'Pattern', speye(nLarge)), ...
  'SolverOptions', options);
tic();
largeResult = solve(largeProblem, 0, 1e-5);
largeElapsed = toc();
fprintf('  spgmr+ilu0 sparse n=%d: %.6f s, setups=%d, solves=%d\n', ...
  nLarge, largeElapsed, largeResult.RawSolution.stats.preconditionerSetupCount, ...
  largeResult.RawSolution.stats.preconditionerSolveCount);
asserts.isequal(largeResult.RawSolution.stats.preconditioner, 'ilu0');
asserts.istrue(abs(largeResult.Solution(1, length(largeResult.Time)) - exp(-1e-5)) < 1e-5);
%=============================================================================
