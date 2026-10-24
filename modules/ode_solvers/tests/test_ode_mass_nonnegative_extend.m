%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('Mass', 2, 'MaxStep', 0.05);
[t, y] = ode45(@(tt, yy) - yy, [0 1], 2, options);
asserts.istrue(abs(y(size(y, 1), 1) - 2 * exp(-0.5)) < 1e-3);
%=============================================================================
options = odeset('NonNegative', 1, 'MaxStep', 0.05);
[t, y] = ode45(@(tt, yy) - 10, [0 1], 1, options);
asserts.istrue(all(y(:) >= 0));
%=============================================================================
sol = ode45(@(tt, yy) - yy, [0 0.5], 1);
sol = odextend(sol, [], 1);
asserts.istrue(abs(sol.x(length(sol.x)) - 1) < 1e-12);
asserts.istrue(abs(sol.y(1, size(sol.y, 2)) - exp(-1)) < 5e-3);
%=============================================================================
