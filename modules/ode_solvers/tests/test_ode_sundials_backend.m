%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
previousRuntime = getenv('NELSON_SUNDIALS_RUNTIME');
runtimeCleanup = onCleanup(@() setenv('NELSON_SUNDIALS_RUNTIME', previousRuntime)); %#ok<NLS0004>
buildAvailable = __ode_sundials_available__();
asserts.istrue(islogical(buildAvailable));
%=============================================================================
disabledModes = {'OFF', '0', 'FALSE'};
for k = 1:length(disabledModes)
  setenv('NELSON_SUNDIALS_RUNTIME', disabledModes{k});
  asserts.isequal(__ode_sundials_available__(), buildAvailable);
  disabledProblem = ode('ODEFcn', @(t, y) - y, 'InitialValue', 1);
  asserts.isequal(disabledProblem.SelectedSolver, 'ode45');
end
%=============================================================================
setenv('NELSON_SUNDIALS_RUNTIME', 'OFF');
fallbackProblem = ode('ODEFcn', @(t, y) - y, 'InitialValue', 1);
asserts.isequal(fallbackProblem.SelectedSolver, 'ode45');
fallbackResult = solve(fallbackProblem, 0, 1);
asserts.isequal(fallbackResult.RawSolution.solver, 'ode45');
asserts.istrue(abs(fallbackResult.SolutionValues(length(fallbackResult.Time)) - exp(-1)) < 5e-4);
%=============================================================================
stiffFallback = ode('Solver', 'stiff');
asserts.isequal(stiffFallback.SelectedSolver, 'ode15s');
nonstiffFallback = ode('Solver', 'nonstiff');
asserts.isequal(nonstiffFallback.SelectedSolver, 'ode45');
autoMassFallback = ode('ODEFcn', @(t, y) y, 'InitialValue', 1, 'MassMatrix', odeMassMatrix(2));
asserts.isequal(autoMassFallback.SelectedSolver, 'ode15s');
implicitFallback = ode('EquationType', 'fullyimplicit');
asserts.isequal(implicitFallback.SelectedSolver, 'ode15i');
asserts.checkerror('ode(''ODEFcn'', @(t, y) -y, ''InitialValue'', 1, ''Solver'', ''cvodesnonstiff'').solve(0, 1);', ...
  _('SUNDIALS backend is not available.'));
%=============================================================================
setenv('NELSON_SUNDIALS_RUNTIME');
available = buildAvailable;
if ~available
  return
end
setenv('NELSON_SUNDIALS_RUNTIME', 'ON');
asserts.isequal(upper(getenv('NELSON_SUNDIALS_RUNTIME')), 'ON');
runtimeOnProblem = ode('ODEFcn', @(t, y) - y, 'InitialValue', 1);
asserts.isequal(runtimeOnProblem.SelectedSolver, 'cvodesnonstiff');
asserts.checkerror('ode(''ODEFcn'', @(t,y,p) p(1)*y, ''InitialValue'', 1, ''Parameters'', 2, ''Sensitivity'', odeSensitivity(''Method'', ''adjoint''), ''Solver'', ''cvodesnonstiff'').solve(0, 0.2);', ... %#ok<NLS0012>
  _('Adjoint sensitivities require ObjectiveFcn or QuadratureFcn.'));
preconditionerError = _('SUNDIALS preconditioner requires an iterative linear solver.');
linearSolverError = sprintf(_('%s has an invalid value.'), 'LinearSolver');
[ok, message] = asserts.checkerror('ode(''ODEFcn'', @(t,y) -y, ''InitialValue'', 1, ''SolverOptions'', nelson.ode.options.CVODESStiff(''LinearSolver'', ''dense'', ''Preconditioner'', ''ilu0'')).solve(0, 0.2);', preconditionerError); %#ok<NLS0012>
asserts.istrue(ok || strcmp(message, linearSolverError));
[ok, message] = asserts.checkerror('ode(''ODEFcn'', @(t,y) -y, ''InitialValue'', 1, ''SolverOptions'', nelson.ode.options.CVODESStiff(''LinearSolver'', ''klu'', ''Preconditioner'', ''ilu0'')).solve(0, 0.2);', preconditionerError); %#ok<NLS0012>
asserts.istrue(ok || strcmp(message, linearSolverError));
setenv('NELSON_SUNDIALS_RUNTIME');
%=============================================================================
