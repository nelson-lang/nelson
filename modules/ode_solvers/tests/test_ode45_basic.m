%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
f = @(t, y) - y;
[t, y] = ode45(f, [0 1], 1);
asserts.istrue(abs(t(1)) < eps);
asserts.istrue(abs(t(length(t)) - 1) < 1e-12);
asserts.istrue(abs(y(size(y, 1), 1) - exp(-1)) < 5e-4);
%=============================================================================
sol = ode45(f, [0 0.5 1], 1, odeset('RelTol', 1e-5));
asserts.istrue(length(sol.x) >= 2);
asserts.istrue(abs(sol.x(1)) < eps);
asserts.istrue(abs(sol.x(length(sol.x)) - 1) < 1e-12);
asserts.istrue(abs(sol.y(1, size(sol.y, 2)) - exp(-1)) < 5e-4);
%=============================================================================
[v, vp] = deval(sol, 0.25);
asserts.istrue(abs(v - exp(-0.25)) < 5e-3);
asserts.istrue(abs(vp + v) < 5e-2);
%=============================================================================
