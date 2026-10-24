%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
problem = ode('ODEFcn', @(t, y) y, 'InitialValue', 1, ...
  'MassMatrix', odeMassMatrix(2), ...
  'Jacobian', odeJacobian(@(t, y) 1), ...
  'SolverOptions', nelson.ode.options.ODE45(odeset('MaxStep', 0.05)));
[fcn, data] = solutionFcn(problem, 0, 0.5);
asserts.istrue(abs(data.Solution(length(data.Solution)) - exp(0.25)) < 5e-4);
asserts.istrue(abs(fcn(0.25) - exp(0.125)) < 5e-4);
asserts.istrue(isa(data, 'nelson.ode.ODEResults'));
asserts.isequal(data.RawSolution.solver, problem.SelectedSolver);
asserts.isequal(size(data.Time, 1), 1);
asserts.isequal(size(data.Solution, 1), 1);
asserts.istrue(abs(deval(data, 0.25) - exp(0.125)) < 5e-4);

saved = saveobj(problem);
loaded = ode.loadobj(saved);
asserts.isequal(loaded.SelectedSolver, problem.SelectedSolver);
asserts.isequal(loaded.InitialValue, problem.InitialValue);

asserts.checkerror('ode(''Sensitivity'', 1, ''ODEFcn'', @(t,y) y, ''InitialValue'', 1)', sprintf(_('%s definition object expected.'), 'Sensitivity'));
asserts.checkerror('ode(''InitialValue'', {1})', _('InitialValue: finite numeric vector expected.'));
asserts.checkerror('ode(''MassMatrix'', {1})', sprintf(_('%s must be numeric or a function handle.'), 'Mass matrix'));
asserts.checkerror('ode(''Jacobian'', {1})', sprintf(_('%s must be numeric or a function handle.'), 'Jacobian'));
%=============================================================================
