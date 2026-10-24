%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('integral2'), -6);
asserts.isequal(nargout('integral2'), 1);
%=============================================================================
% product over the unit square
R = integral2(@(x, y) x .* y, 0, 1, 0, 1);
asserts.isapprox(R, 0.25, 1e-10);
%=============================================================================
% sum of squares over the unit square
R = integral2(@(x, y) x .^ 2 + y .^ 2, 0, 1, 0, 1);
asserts.isapprox(R, 2 / 3, 1e-10);
%=============================================================================
% constant integrand gives the area
R = integral2(@(x, y) ones(size(x)), 0, 2, 0, 3);
asserts.isapprox(R, 6, 1e-10);
%=============================================================================
% integrable singularity inside the square
R = integral2(@(x, y) 1 ./ sqrt(x + y), 0, 1, 0, 1);
asserts.isapprox(R, 4 / 3 * (2 ^ 1.5 - 2), 1e-6);
%=============================================================================
% triangular region with a function-handle upper y bound
R = integral2(@(x, y) x .* y, 0, 1, 0, @(x) x);
asserts.isapprox(R, 0.125, 1e-9);
%=============================================================================
% triangular region with a function-handle lower y bound
R = integral2(@(x, y) ones(size(x)), 0, 1, @(x) x, 1);
asserts.isapprox(R, 0.5, 1e-9);
%=============================================================================
% separable exponential over an infinite quarter plane
R = integral2(@(x, y) exp(-x - y), 0, Inf, 0, Inf);
asserts.isapprox(R, 1, 1e-8);
%=============================================================================
% oscillatory integrand over a square
R = integral2(@(x, y) sin(x + y), 0, pi, 0, pi);
asserts.isapprox(R, 0, 0, 1e-8);
%=============================================================================
% tolerance options
R = integral2(@(x, y) x .* y, 0, 1, 0, 1, 'RelTol', 1e-10, 'AbsTol', 1e-12);
asserts.isapprox(R, 0.25, 1e-10);
%=============================================================================
asserts.checkerror('integral2(3, 0, 1, 0, 1)', _('First argument must be a function handle.'), 'Nelson:special_functions:funArgNotHandle');
%=============================================================================
