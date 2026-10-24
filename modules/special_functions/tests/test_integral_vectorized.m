%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documented example: integrand written for scalar inputs
fun = @(x) 2 * x - x ^ 2;
q = integral(fun, 0, 1, 'Vectorized', false);
asserts.isapprox(q, 2 / 3, 1e-10);
%=============================================================================
q = integral(@(x) exp(-x ^ 2), 0, Inf, 'Vectorized', 0);
asserts.isapprox(q, sqrt(pi) / 2, 1e-9);
%=============================================================================
q = integral(@(x) abs(x - 0.3), 0, 1, 'Vectorized', false, 'Waypoints', 0.3);
asserts.isapprox(q, 0.29, 1e-12);
%=============================================================================
q = integral(@(x) 2 * x - x ^ 2, 0, 1, 'Vectorized', true, 'ArrayValued', true);
asserts.isapprox(q, 2 / 3, 1e-10);
%=============================================================================
% ArrayValued takes precedence over Vectorized
q = integral(@(x) [x, 2 * x], 0, 1, 'Vectorized', false, 'ArrayValued', true);
asserts.isapprox(q, [0.5, 1], 1e-10);
%=============================================================================
% documented example of integral2 (log(x^2 + y^2) over [0, 2] x [0, 2])
fun = @(x, y) log(x ^ 2 + y ^ 2);
q = integral2(fun, 0, 2, 0, 2, 'Vectorized', false);
asserts.isapprox(q, 12 * log(2) - 12 + 2 * pi, 1e-6);
%=============================================================================
q = integral2(@(x, y) x * y, 0, 1, 0, @(x) x, 'Vectorized', false);
asserts.isapprox(q, 0.125, 1e-9);
%=============================================================================
asserts.checkerror('integral(@(x) x, 0, 1, ''Vectorized'', ''a'')', sprintf(_('%s must be a logical scalar.'), 'Vectorized'));
asserts.checkerror('integral(@(x) x, 0, 1, ''Vectorized'', ''a'')', sprintf(_('%s must be a logical scalar.'), 'Vectorized'), 'Nelson:integral:Vectorized');
asserts.checkerror('integral2(@(x, y) x, 0, 1, 0, 1, ''Vectorized'', [1 0])', sprintf(_('%s must be a logical scalar.'), 'Vectorized'), 'Nelson:integral2:Vectorized');
%=============================================================================
