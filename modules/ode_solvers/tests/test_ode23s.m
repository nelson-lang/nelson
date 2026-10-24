%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
odeCheckSolverForTest(@ode23s);
%=============================================================================
% Stiff accuracy: the modified Rosenbrock (2,3) must track the requested
% tolerance on stiff problems (guards against a low-order approximation).
% Stiff linear system with a fast (lambda = -100) and a slow (lambda = -1) mode.
A = [-100 1; 0 -1];
yexact = @(t)[exp(-100 * t) + (exp(-t) - exp(-100 * t)) / 99; exp(-t)];
solLoose = ode23s(@(t, y) A * y, [0 1], [1; 1], odeset('RelTol', 1e-3, 'AbsTol', 1e-6));
asserts.istrue(max(abs(solLoose.y(:, end) - yexact(1))) < 1e-2);
solTight = ode23s(@(t, y) A * y, [0 1], [1; 1], odeset('RelTol', 1e-6, 'AbsTol', 1e-9));
asserts.istrue(max(abs(solTight.y(:, end) - yexact(1))) < 1e-4);
%=============================================================================
% Very stiff decay (lambda = -1000) resolved to a tight tolerance.
Ad = [-1000 0; 0 -1];
solDecay = ode23s(@(t, y) Ad * y, [0 0.5], [1; 1], odeset('RelTol', 1e-6, 'AbsTol', 1e-9));
asserts.istrue(abs(solDecay.y(2, end) - exp(-0.5)) < 1e-4);
%=============================================================================

