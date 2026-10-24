%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('Events', @odeTestEventHalf, 'MaxStep', 0.05);
sol = ode45(@(t, y) 1, [0 1], 0, options);
names = fieldnames(sol);
asserts.isequal(names{1}, 'solver');
asserts.isequal(names{2}, 'extdata');
asserts.isequal(names{3}, 'x');
asserts.isequal(names{4}, 'y');
asserts.isequal(names{5}, 'xe');
asserts.isequal(names{6}, 'ye');
asserts.isequal(names{7}, 'ie');
asserts.isequal(names{8}, 'stats');
asserts.isequal(names{9}, 'idata');
asserts.istrue(abs(sol.x(length(sol.x)) - 0.5) < 1e-10);
asserts.istrue(abs(sol.y(length(sol.y)) - 0.5) < 1e-10);
asserts.istrue(abs(sol.xe(1) - 0.5) < 1e-10);
asserts.istrue(abs(sol.ye(1) - 0.5) < 1e-10);
asserts.isequal(sol.ie(1), 1);
asserts.istrue(abs(sol.stats.tfinal - sol.x(length(sol.x))) < 1e-12);

idataNames = fieldnames(sol.idata);
asserts.isequal(idataNames{1}, 'f3d');
asserts.isequal(idataNames{2}, 'idxNonNegative');
%=============================================================================
vectorOptions = odeset('Events', @odeTestFirstStateEvent, 'MaxStep', 0.05);
[tv, yv, tev, yev, iev] = ode45(@(t, y)[1; 2], [0 1], [0; 0], vectorOptions);
asserts.isequal(size(yv, 2), 2);
asserts.isequal(size(yev), [1 2]);
asserts.istrue(abs(tev(1) - 0.5) < 1e-10);
asserts.istrue(max(abs(yev(1, :) - [0.5 1])) < 1e-10);
asserts.isequal(iev(1), 1);

vectorSol = ode45(@(t, y)[1; 2], [0 1], [0; 0], vectorOptions);
asserts.isequal(size(vectorSol.ye), [2 1]);
asserts.istrue(max(abs(vectorSol.ye(:, 1) - [0.5; 1])) < 1e-10);

vectorProblem = ode('ODEFcn', @(t, y)[1; 2], ...
  'InitialValue', [0; 0], ...
  'EventDefinition', odeEvent(@odeTestFirstStateEvent));
vectorResult = solve(vectorProblem, 0, 1);
asserts.isequal(size(vectorResult.EventSolution), [2 1]);
asserts.istrue(max(abs(vectorResult.EventSolution(:, 1) - [0.5; 1])) < 1e-10);
%=============================================================================
