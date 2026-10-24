%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
requested = 0:0.05:0.5;
options = odeset('InitialStep', 0.002, 'MaxStep', 0.002, 'RelTol', 1e-5, 'AbsTol', 1e-7);
rhs = @(t, y)[1i * 30 * y(1); -0.5 * y(2)];
y0 = [1; 2i];
%=============================================================================
sol = ode45(rhs, requested, y0, options);
asserts.istrue(length(sol.x) > length(requested));
%=============================================================================
[t, y] = ode45(rhs, requested, y0, options);
asserts.isequal(t(:).', requested);
reference = deval(sol, requested);
err = abs(y.' - reference);
asserts.istrue(max(err(:)) < 1e-8);
%=============================================================================
