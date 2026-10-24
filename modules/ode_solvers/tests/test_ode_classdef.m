%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
problem = ode('ODEFcn', @(t, y) - y, 'InitialValue', 1, 'Solver', 'ode45');
asserts.isequal(problem.SelectedSolver, 'ode45');
result = solve(problem, 0, 1);
asserts.istrue(isa(result, 'nelson.ode.ODEResults'));
asserts.istrue(abs(result.Solution(1, size(result.Solution, 2)) - exp(-1)) < 5e-4);
asserts.istrue(abs(deval(result, 0.5) - exp(-0.5)) < 5e-3);
f = solutionFcn(problem, 0, 1);
y = f(0.5);
asserts.istrue(abs(y - exp(-0.5)) < 5e-3);
%=============================================================================
problem = ode('ODEFcn', @(t, y) - y, 'InitialTime', 2, 'InitialValue', 1, 'Solver', 'ode45');
result = solve(problem, 3);
asserts.istrue(abs(result.Time(1) - 2) < 1e-12);
asserts.istrue(abs(result.Time(length(result.Time)) - 3) < 1e-12);
asserts.istrue(abs(result.SolutionValues(length(result.Time)) - exp(-1)) < 5e-4);
f = solutionFcn(problem, 3);
asserts.istrue(abs(f(2.5) - exp(-0.5)) < 5e-3);
explicit = solve(problem, 0, 1);
asserts.istrue(abs(explicit.Time(1)) < 1e-12);
asserts.istrue(abs(explicit.Time(length(explicit.Time)) - 1) < 1e-12);
asserts.checkerror('ode(''InitialTime'', [0 1])', sprintf(_('%s must be a finite scalar.'), 'InitialTime'));
asserts.checkerror('solve(problem, [3 4])', sprintf(_('%s must be a finite scalar.'), 'FinalTime'));
%=============================================================================
