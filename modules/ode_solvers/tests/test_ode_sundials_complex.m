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
buildAvailable = odeRequireSundialsForTest();
if ~buildAvailable
  return
end
setenv('NELSON_SUNDIALS_RUNTIME', 'ON');
%=============================================================================
complexInitial = 1 + 0.1i;
complexNonstiffProblem = ode('ODEFcn', @(t, y) 1i * y, ...
  'InitialValue', complexInitial, ...
  'RelativeTolerance', 1e-8, ...
  'AbsoluteTolerance', 1e-10, ...
  'Solver', 'cvodesnonstiff');
complexNonstiffResult = solve(complexNonstiffProblem, 0, 1);
asserts.isequal(complexNonstiffResult.RawSolution.solver, 'cvodesnonstiff');
asserts.isfalse(isreal(complexNonstiffResult.Solution));
asserts.istrue(abs(complexNonstiffResult.SolutionValues(length(complexNonstiffResult.Time)) - complexInitial * exp(1i)) < 1e-4); %#ok<NLS0012>
%=============================================================================
complexMassProblem = ode('ODEFcn', @(t, y) 1i * y, ...
  'InitialValue', complexInitial, ...
  'RelativeTolerance', 1e-8, ...
  'AbsoluteTolerance', 1e-10, ...
  'MassMatrix', odeMassMatrix(1 + 1i), ...
  'Solver', 'cvodesstiff');
complexMassResult = solve(complexMassProblem, 0, 1);
asserts.isfalse(isreal(complexMassResult.Solution));
asserts.istrue(abs(complexMassResult.SolutionValues(length(complexMassResult.Time)) - complexInitial * exp((0.5 + 0.5i))) < 1e-4); %#ok<NLS0012>
%=============================================================================
complexIdasOptions = nelson.ode.options.IDAS('ComputeConsistentInitialConditions', false);
complexIdasProblem = ode('EquationType', 'fullyimplicit', ...
  'ODEFcn', @(t, y, yp) yp + 1i * y, ...
  'InitialValue', complexInitial, ...
  'InitialSlope', -1i * complexInitial, ...
  'RelativeTolerance', 1e-8, ...
  'AbsoluteTolerance', 1e-10, ...
  'SolverOptions', complexIdasOptions);
complexIdasResult = solve(complexIdasProblem, 0, 1);
asserts.isequal(complexIdasResult.RawSolution.solver, 'idas');
asserts.isfalse(isreal(complexIdasResult.Solution));
asserts.istrue(abs(complexIdasResult.SolutionValues(length(complexIdasResult.Time)) - complexInitial * exp(-1i)) < 1e-4); %#ok<NLS0012>
%=============================================================================
