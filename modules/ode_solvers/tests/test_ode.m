%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
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
asserts.checkerror('ode(''InitialTime'', [0 1])', sprintf(_('%s must be a finite scalar.'), 'InitialTime'));
%=============================================================================
