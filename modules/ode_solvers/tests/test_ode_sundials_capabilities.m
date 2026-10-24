%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
capabilities = __ode_sundials_capabilities__();
fields = {'available'; 'sparseMatrix'; 'spgmr'; 'spfgmr'; 'spbcgs'; ...
  'sptfqmr'; 'pcg'; 'klu'; 'kinsol'};
asserts.isequal(fieldnames(capabilities), fields);
for k = 1:length(fields)
  asserts.istrue(islogical(capabilities.(fields{k})));
end
asserts.isequal(capabilities.available, __ode_sundials_available__());

previousRuntime = getenv('NELSON_SUNDIALS_RUNTIME');
runtimeCleanup = onCleanup(@() setenv('NELSON_SUNDIALS_RUNTIME', previousRuntime));
setenv('NELSON_SUNDIALS_RUNTIME', 'ON');

if ~capabilities.available
  asserts.checkerror('ode(''ODEFcn'', @(t, y) -y, ''InitialValue'', 1, ''Solver'', ''cvodesstiff'').solve(0, 1);', ...
    _('SUNDIALS backend is not available.'));
  return
end

kluOptions = nelson.ode.options.CVODESStiff('LinearSolver', 'klu');
kluProblem = ode('ODEFcn', @(t, y)[-20 * y(1); -30 * y(2)], ...
  'InitialValue', [1; 2], ...
  'RelativeTolerance', 1e-8, ...
  'AbsoluteTolerance', 1e-10, ...
  'Jacobian', odeJacobian(@(t, y) sparse([-20 0; 0 -30])), ...
  'SolverOptions', kluOptions);

if capabilities.klu
  kluResult = solve(kluProblem, 0, 0.2);
  asserts.isequal(kluResult.RawSolution.stats.linearSolver, 'klu');
  asserts.isequal(kluResult.RawSolution.stats.preconditioner, 'none');
  asserts.istrue(max(abs(kluResult.Solution(:, length(kluResult.Time)) ...
    - [exp(-4); 2 * exp(-6)])) < 1e-4);
else
  asserts.checkerror('solve(kluProblem, 0, 0.2);', ...
    _('SUNDIALS KLU linear solver is not available.'));
end
%=============================================================================
