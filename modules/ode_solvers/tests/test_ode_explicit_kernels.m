%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('RelTol', 1e-6, 'AbsTol', 1e-9, 'MaxStep', 0.1);
sol23 = ode23(@(t, y) y, [0 1], 1, options);
sol45 = ode45(@(t, y) y, [0 1], 1, options);
asserts.isequal(sol23.solver, 'ode23');
asserts.isequal(sol45.solver, 'ode45');
asserts.istrue(abs(sol23.y(length(sol23.y)) - exp(1)) < 2e-4);
asserts.istrue(abs(sol45.y(length(sol45.y)) - exp(1)) < 2e-6);
asserts.istrue(sol23.stats.nfevals >= 4 * sol23.stats.nsteps);
asserts.istrue(sol45.stats.nfevals >= 7 * sol45.stats.nsteps);
sol78 = ode78(@(t, y) y, [0 1], 1, options);
sol89 = ode89(@(t, y) y, [0 1], 1, options);
sol113 = ode113(@(t, y) y, [0 1], 1, options);
asserts.isequal(sol78.solver, 'ode78');
asserts.isequal(sol89.solver, 'ode89');
asserts.isequal(sol113.solver, 'ode113');
asserts.istrue(abs(sol78.y(length(sol78.y)) - exp(1)) < 2e-7);
asserts.istrue(abs(sol89.y(length(sol89.y)) - exp(1)) < 2e-7);
asserts.istrue(abs(sol113.y(length(sol113.y)) - exp(1)) < 2e-6);
% Verner pairs: ten stages for the 7(6) formula, thirteen for the 8(7).
asserts.istrue(sol78.stats.nfevals >= 10 * sol78.stats.nsteps);
asserts.istrue(sol89.stats.nfevals >= 13 * sol89.stats.nsteps);
asserts.istrue(sol89.stats.nfevals > sol78.stats.nfevals);
asserts.istrue(sol113.stats.nfevals >= sol113.stats.nsteps);
asserts.istrue(sol113.stats.nfevals < sol78.stats.nfevals);

[t23, y23] = ode23(@(t, y)[y(2); -y(1)], [0 pi / 4 pi / 2], [0; 1], options);
[t45, y45] = ode45(@(t, y)[y(2); -y(1)], [0 pi / 4 pi / 2], [0; 1], options);
asserts.istrue(max(abs(y23(:, 1) - sin(t23))) < 2e-4);
asserts.istrue(max(abs(y45(:, 1) - sin(t45))) < 2e-6);
%=============================================================================
