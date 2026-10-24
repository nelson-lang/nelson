%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[t, y] = ode45(@(t, y) y, [1 0.5 0], exp(1));
asserts.isequal(t, [1; 0.5; 0]);
asserts.istrue(max(abs(y - exp(t))) < 5e-4);

options = odeset('Mass', 2, 'MaxStep', 0.05);
[tm, ym] = ode45(@(t, y) y, [0 1], 1, options);
asserts.istrue(abs(ym(length(ym)) - exp(0.5)) < 5e-4);

options = odeset('Mass', @(t, y) 2, 'MaxStep', 0.05);
[tf, yf] = ode45(@(t, y) y, [0 1], 1, options);
asserts.istrue(abs(yf(length(yf)) - exp(0.5)) < 5e-4);

sol = ode45(@(t, y) y, [0 0.5], 1);
sol2 = odextend(sol, [], 1);
asserts.istrue(abs(sol2.x(length(sol2.x)) - 1) < 1e-12);
asserts.istrue(abs(sol2.y(length(sol2.y)) - exp(1)) < 5e-4);
asserts.istrue(sol2.stats.tfinal == 1);
%=============================================================================
