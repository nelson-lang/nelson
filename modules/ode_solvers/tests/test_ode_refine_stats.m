%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('Refine', 4, 'InitialStep', 0.5, 'MaxStep', 0.5);
[t, y] = ode45(@(t, y) y, [0 1], 1, options);
asserts.isequal(length(t), 9);
asserts.isequal(t.', 0:0.125:1);
asserts.istrue(abs(y(length(y)) - exp(1)) < 1e-5);
%=============================================================================
sol = ode45(@(t, y) y, [0 1], 1, options);
asserts.isequal(length(sol.x), 3);
asserts.isfalse(isfield(sol.extdata, 'internal'));
asserts.istrue(abs(deval(sol, 0.5) - exp(0.5)) < 1e-5);
%=============================================================================
options = odeset('Refine', 1, 'InitialStep', 0.5, 'MaxStep', 0.5);
[t, y] = ode45(@(t, y) y, [0 1], 1, options);
asserts.isequal(length(t), 3);
asserts.isequal(t.', [0 0.5 1]);
%=============================================================================
baseOptions = odeset('InitialStep', 0.5, 'MaxStep', 0.5);
asserts.isequal(length(ode45(@(t, y) y, [0 1], 1, baseOptions).x), 3);
asserts.isequal(length(ode78(@(t, y) y, [0 1], 1, baseOptions).x), 3);
asserts.isequal(length(ode89(@(t, y) y, [0 1], 1, baseOptions).x), 3);
%=============================================================================
options = odeset('Stats', 'on', 'InitialStep', 0.5, 'MaxStep', 0.5);
txt = evalc('[t, y] = ode45(@(t, y) y, [0 1], 1, options);');
asserts.istrue(contains(txt, 'successful steps'));
asserts.istrue(contains(txt, 'failed attempts'));
asserts.istrue(contains(txt, 'function evaluations'));
%=============================================================================
asserts.checkerror('ode45(@(t, y) y, [0 1], 1, odeset(''Refine'', 1.5))', _('Refine must be an integer scalar.'));
%=============================================================================
