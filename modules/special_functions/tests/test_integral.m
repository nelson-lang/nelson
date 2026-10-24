%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('integral'), -4);
asserts.isequal(nargout('integral'), 1);
%=============================================================================
% polynomial
R = integral(@(x) x .^ 2, 0, 1);
asserts.isapprox(R, 1 / 3, 1e-10);
%=============================================================================
R = integral(@(x) x .^ 3 - 2 * x + 1, -1, 2);
asserts.isapprox(R, 3.75, 1e-10);
%=============================================================================
% trigonometric
R = integral(@sin, 0, pi);
asserts.isapprox(R, 2, 1e-10);
%=============================================================================
R = integral(@(x) cos(x), 0, pi / 2);
asserts.isapprox(R, 1, 1e-10);
%=============================================================================
% oscillatory
R = integral(@(x) sin(100 * x), 0, 2 * pi);
asserts.isapprox(R, 0, 0, 1e-8);
%=============================================================================
% exponential decay on a semi-infinite interval
R = integral(@(x) exp(-x), 0, Inf);
asserts.isapprox(R, 1, 1e-9);
%=============================================================================
% Gaussian on a semi-infinite interval (sqrt(pi)/2)
R = integral(@(x) exp(-x .^ 2), 0, Inf);
asserts.isapprox(R, sqrt(pi) / 2, 1e-9);
%=============================================================================
% Gaussian on the whole real line (sqrt(pi))
R = integral(@(x) exp(-x .^ 2), -Inf, Inf);
asserts.isapprox(R, sqrt(pi), 1e-8);
%=============================================================================
% Cauchy density integrates to pi over the whole real line
R = integral(@(x) 1 ./ (1 + x .^ 2), -Inf, Inf);
asserts.isapprox(R, pi, 1e-8);
%=============================================================================
% semi-infinite left limit
R = integral(@(x) exp(x), -Inf, 0);
asserts.isapprox(R, 1, 1e-9);
%=============================================================================
% integrable singularity at the left endpoint
R = integral(@(x) 1 ./ sqrt(x), 0, 1);
asserts.isapprox(R, 2, 1e-6);
%=============================================================================
% integrable logarithmic singularity
R = integral(@(x) log(x), 0, 1);
asserts.isapprox(R, -1, 1e-6);
%=============================================================================
% sine cardinal
R = integral(@(x) sin(x) ./ x, 0, 1);
asserts.isapprox(R, 0.946083070367183, 1e-10);
%=============================================================================
% reversed limits change the sign
R = integral(@(x) x .^ 2, 1, 0);
asserts.isapprox(R, -1 / 3, 1e-10);
%=============================================================================
% zero-length interval
R = integral(@(x) x .^ 2, 3, 3);
asserts.isequal(R, 0);
%=============================================================================
% complex-valued integrand
R = integral(@(x) exp(1i * x), 0, pi);
asserts.isapprox(R, 2i, 1e-10, 1e-10);
%=============================================================================
% array-valued integrand
R = integral(@(x)[1; 1] .* x, 0, 1, 'ArrayValued', true);
asserts.isapprox(R, [0.5; 0.5], 1e-10);
%=============================================================================
R = integral(@(x)[x; x .^ 2; sin(x)], 0, 1, 'ArrayValued', true);
asserts.isapprox(R, [0.5; 1 / 3; 1 - cos(1)], 1e-10);
%=============================================================================
% tolerance options
R = integral(@(x) exp(-x .^ 2), 0, Inf, 'RelTol', 1e-12, 'AbsTol', 1e-14);
asserts.isapprox(R, sqrt(pi) / 2, 1e-11);
%=============================================================================
asserts.checkerror('integral(3, 0, 1)', _('First argument must be a function handle.'), 'Nelson:special_functions:funArgNotHandle');
%=============================================================================
asserts.checkerror('integral(@sin, 0, 1, ''RelTol'')', _('Options must be given as name/value pairs.'), 'Nelson:integral:optionsPairs');
%=============================================================================
asserts.checkerror('integral(@sin, 0, 1, ''Foo'', 1)', sprintf(_('Unknown option name: %s'), 'Foo'), 'Nelson:integral:unknownOption');
%=============================================================================
