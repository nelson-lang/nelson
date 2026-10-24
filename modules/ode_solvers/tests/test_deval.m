%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sol = ode45(@(t, y) - y, [0 1], 1, odeset('MaxStep', 0.1));
[y, yp] = deval(sol, 0.25);
asserts.istrue(abs(y - exp(-0.25)) < 5e-3);
asserts.istrue(abs(yp + y) < 5e-2);
asserts.checkerror('deval(sol, 2)', _('Evaluation points must lie inside the solution interval.'));
%=============================================================================
% swapped argument order deval(x, sol)
ySwapped = deval(0.25, sol);
asserts.istrue(abs(ySwapped - y) < 1e-12);
%=============================================================================
% component selection deval(sol, x, idx)
systemSol = ode45(@(t, y)[y(2); -y(1)], [0 10], [1; 0]);
full = deval(systemSol, 1.5);
second = deval(systemSol, 1.5, 2);
asserts.istrue(abs(second - full(2)) < 1e-12);
firstSwapped = deval(1.5, systemSol, 1);
asserts.istrue(abs(firstSwapped - full(1)) < 1e-12);
asserts.checkerror('deval(systemSol, 1.5, 3)', sprintf(_('%s indices are invalid.'), 'Component'));
%=============================================================================
