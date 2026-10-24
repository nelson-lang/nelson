%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sol = ode45(@(t, y) y, [0 1], 1);
names = fieldnames(sol);
asserts.isequal(names{1}, 'solver');
asserts.isequal(names{2}, 'extdata');
asserts.isequal(names{3}, 'x');
asserts.isequal(names{4}, 'y');
asserts.isequal(names{5}, 'stats');
asserts.isequal(names{6}, 'idata');
asserts.isfalse(isfield(sol, 'xe'));
asserts.isfalse(isfield(sol, 'ye'));
asserts.isfalse(isfield(sol, 'ie'));

statsNames = fieldnames(sol.stats);
asserts.isequal(statsNames{1}, 'nsteps');
asserts.isequal(statsNames{2}, 'nfailed');
asserts.isequal(statsNames{3}, 'nfevals');
asserts.isequal(statsNames{4}, 'tfinal');
asserts.istrue(abs(sol.stats.tfinal - 1) < 1e-12);

value = deval(sol, 0.5);
asserts.istrue(abs(value - exp(0.5)) < 1e-4);
[values, slopes] = deval(sol, [0; 0.5; 1]);
asserts.isequal(size(values), [1 3]);
asserts.isequal(size(slopes), [1 3]);
asserts.istrue(max(abs(values - [1 exp(0.5) exp(1)])) < 5e-3);

systemSol = ode45(@(t, y)[y(2); -y(1)], [0 1], [0; 1]);
[systemValues, systemSlopes] = deval(systemSol, [0; 0.5; 1]);
asserts.isequal(size(systemValues), [2 3]);
asserts.isequal(size(systemSlopes), [2 3]);
reverseSol = ode45(@(t, y) y, [1 0], exp(1));

extended = odextend(sol, [], 2);
asserts.isfalse(isfield(extended, 'xe'));
asserts.isfalse(isfield(extended, 'ye'));
asserts.isfalse(isfield(extended, 'ie'));
asserts.istrue(abs(extended.x(length(extended.x)) - 2) < 1e-12);
sameInterval = odextend(sol, [], 0.5);
asserts.isfalse(isfield(sameInterval, 'xe'));
asserts.isfalse(isfield(sameInterval, 'ye'));
asserts.isfalse(isfield(sameInterval, 'ie'));
asserts.istrue(abs(sameInterval.x(length(sameInterval.x)) - 1) < 1e-12);
asserts.checkerror('odextend(sol, [], -1)', _('ODE solution cannot be extended in the requested direction.'));

reverseExtended = odextend(reverseSol, [], -1);
asserts.istrue(abs(reverseExtended.x(length(reverseExtended.x)) + 1) < 1e-12);
asserts.checkerror('odextend(reverseSol, [], 2)', _('ODE solution cannot be extended in the requested direction.'));

asserts.checkerror('deval(sol, -0.1)', _('Evaluation points must lie inside the solution interval.'));
asserts.checkerror('deval(sol, 1.1)', _('Evaluation points must lie inside the solution interval.'));
asserts.checkerror('deval(reverseSol, 1.1)', _('Evaluation points must lie inside the solution interval.'));
asserts.checkerror('deval(reverseSol, -0.1)', _('Evaluation points must lie inside the solution interval.'));
%=============================================================================
