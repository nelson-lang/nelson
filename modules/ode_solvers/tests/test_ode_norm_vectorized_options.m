%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
options = odeset('NormControl', 'on', 'RelTol', 1e-5, 'AbsTol', 1e-8);
[t, y] = ode45(@(t, y)[y(1); -2 * y(2)], [0 1], [1; 1], options);
asserts.istrue(abs(y(size(y, 1), 1) - exp(1)) < 1e-4);
asserts.istrue(abs(y(size(y, 1), 2) - exp(-2)) < 1e-4);
%=============================================================================
clear global ODE_VECTORIZED_STIFF_COLUMNS;
global ODE_VECTORIZED_STIFF_COLUMNS;
ODE_VECTORIZED_STIFF_COLUMNS = [];
options = odeset('Vectorized', 'on', 'MaxStep', 0.05, 'RelTol', 1e-4, 'AbsTol', 1e-7);
sol = ode15s(@odeVectorizedStiffRhs, [0 0.03], [1; 2], options);
asserts.istrue(abs(sol.y(1, length(sol.x)) - exp(-3)) < 5e-3);
asserts.istrue(abs(sol.y(2, length(sol.x)) - 2 * exp(-3)) < 1e-2);
asserts.istrue(any(ODE_VECTORIZED_STIFF_COLUMNS > 1));
%=============================================================================
clear global ODE_VECTORIZED_PATTERN_COLUMNS;
global ODE_VECTORIZED_PATTERN_COLUMNS;
ODE_VECTORIZED_PATTERN_COLUMNS = [];
pattern = [1 0 0; 0 1 0; 0 0 0];
options = odeset('Vectorized', 'on', 'JPattern', pattern, 'MaxStep', 0.05, 'RelTol', 1e-4, 'AbsTol', 1e-7);
sol = ode15s(@odeVectorizedPatternRhs, [0 0.03], [1; 2; 3], options);
asserts.istrue(abs(sol.y(1, length(sol.x)) - exp(-3)) < 5e-3);
asserts.istrue(abs(sol.y(2, length(sol.x)) - 2 * exp(-3)) < 1e-2);
asserts.istrue(abs(sol.y(3, length(sol.x)) - 3) < 1e-12);
asserts.istrue(any(ODE_VECTORIZED_PATTERN_COLUMNS == 2));
asserts.isfalse(any(ODE_VECTORIZED_PATTERN_COLUMNS == 3));
%=============================================================================
