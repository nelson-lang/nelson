%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
odeCheckSolverForTest(@ode15s);
%=============================================================================
% Stiff accuracy: the BDF kernel must track the requested tolerance on stiff
% problems (guards the predictor-based truncation error estimate).
A = [-100 1; 0 -1];
yexact = @(t)[exp(-100 * t) + (exp(-t) - exp(-100 * t)) / 99; exp(-t)];
solLoose = ode15s(@(t, y) A * y, [0 1], [1; 1], odeset('RelTol', 1e-3, 'AbsTol', 1e-6));
asserts.istrue(max(abs(solLoose.y(:, end) - yexact(1))) < 1e-2);
solTight = ode15s(@(t, y) A * y, [0 1], [1; 1], odeset('RelTol', 1e-6, 'AbsTol', 1e-9));
asserts.istrue(max(abs(solTight.y(:, end) - yexact(1))) < 1e-4);
% The variable-order kernel must reach tight tolerances without the step
% explosion of a fixed low-order formula.
asserts.istrue(solTight.stats.nsteps < 400);
%=============================================================================
% An inexact user-supplied Jacobian must not degrade the solution: the
% modified-Newton iteration has to converge on the implicit BDF equation.
wrongJac = ode15s(@(t, y) 1, [0 1], 0, odeset('Jacobian', -1, 'RelTol', 1e-3, 'AbsTol', 1e-6));
asserts.istrue(abs(wrongJac.y(end) - 1) < 1e-4);
%=============================================================================

