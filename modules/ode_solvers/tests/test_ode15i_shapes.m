%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
residual = @(t, y, yp) yp + y;
[t, y] = ode15i(residual, [0 0.25 0.5], 1, -1);
asserts.isequal(size(t), [3 1]);
asserts.isequal(size(y), [3 1]);
asserts.istrue(max(abs(y - exp(-t))) < 5e-4);

sol = ode15i(residual, [0 0.5], 1, -1);
asserts.isequal(sol.solver, 'ode15i');
asserts.istrue(isfield(sol, 'stats'));
asserts.istrue(isfield(sol, 'idata'));
value = deval(sol, 0.25);
asserts.istrue(abs(value - exp(-0.25)) < 5e-4);
%=============================================================================
