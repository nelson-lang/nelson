%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documented example: interpolate the integral of 1 + cos(x)^2
f = @(x) 1 + cos(x) .^ 2;
F = integralInterpolant(f, 0, 5);
asserts.isequal(class(F), 'integralInterpolant');
asserts.isequal(F.Integrand, f);
asserts.isequal(F.LowerLimit, 0);
asserts.isequal(F.UpperLimit, 5);
asserts.isapprox(F.Integral, 7.5 + sin(10) / 4, 1e-12);
asserts.istrue(F.ErrorBound >= 0 && F.ErrorBound < 1e-8);
asserts.isequal(F.AbsoluteTolerance, 1e-10);
asserts.isequal(F.RelativeTolerance, 1e-6);
asserts.isequal(F.Subintervals(1), 0);
asserts.isequal(F.Subintervals(end), 5);
asserts.istrue(isrow(F.Subintervals) && all(diff(F.Subintervals) > 0));
xq = linspace(1, 3, 5);
Fq = F(xq);
asserts.isequal(size(Fq), [1, 5]);
asserts.isapprox(Fq, 1.5 * xq + sin(2 * xq) / 4, 1e-12);
asserts.isapprox(round(Fq * 1e4) / 1e4, [1.7273, 2.2853, 2.8108, 3.5103, 4.4301], 1e-12);
Fq = F(xq.');
asserts.isequal(size(Fq), [5, 1]);
asserts.isapprox(F(F.Subintervals), 1.5 * F.Subintervals + sin(2 * F.Subintervals) / 4, 1e-12);
%=============================================================================
% documented example: improper integral
f = @(x) x .^ 5 .* exp(-x) .* sin(x);
F = integralInterpolant(f, 0, Inf, 'RelativeTolerance', 1e-8, 'AbsoluteTolerance', 1e-13);
asserts.isequal(F.UpperLimit, Inf);
asserts.isequal(F.Subintervals(end), Inf);
Fq = F([0, Inf]);
asserts.isequal(Fq(1), 0);
asserts.isapprox(Fq(2), -15, 1e-8);
asserts.isapprox(F.Integral, -15, 1e-8);
%=============================================================================
% documented example: array-valued integrand, partial sums on the mesh
k = 1:5;
f = @(x) sin(k * x);
F = integralInterpolant(f, 0, 1, 'ArrayValued', true);
asserts.isapprox(F.Integral, (1 - cos(k)) ./ k, 1e-10);
partialSums = F(F.Subintervals(end - 5:end));
asserts.isequal(size(partialSums), [6, 5]);
x = F.Subintervals(end - 5:end).';
asserts.isapprox(partialSums, (1 - cos(x * k)) ./ k, 1e-10);
asserts.isapprox(round(partialSums(end, :) * 1e4) / 1e4, [0.4597, 0.7081, 0.6633, 0.4134, 0.1433], 1e-12);
%=============================================================================
% documented example: comparison with a gridded interpolant
F = integralInterpolant(@(x) x .^ x, 1, 2);
x = linspace(1, 2, 11);
G = griddedInterpolant(x, F(x), 'cubic');
asserts.isapprox(F(1.88), 1.615564488963915, 1e-10);
asserts.isapprox(G(1.88), 1.6154, 1e-4);
%=============================================================================
% backward integration: F(x) is the integral from the lower limit 2 to x
F = integralInterpolant(@(x) x, 2, 0);
asserts.isapprox(F.Integral, -2, 1e-12);
asserts.isapprox(F([1, 0, 2]), [-1.5, -2, 0], 1e-14);
asserts.isequal(F.Subintervals(1), 2);
asserts.isequal(F.Subintervals(end), 0);
%=============================================================================
% query points outside the integration interval
F = integralInterpolant(@(x) x, 0, 1);
asserts.isequal(isnan(F([-1, 0.5, 2, NaN])), [true, false, true, true]);
asserts.isequal(F([]), zeros(0, 0));
%=============================================================================
% waypoints are part of the subintervals
F = integralInterpolant(@(x) abs(x - 0.3), 0, 1, 'Waypoints', 0.3);
asserts.istrue(any(F.Subintervals == 0.3));
asserts.isapprox(F([0, 0.3, 0.5, 1]), [0, 0.045, 0.065, 0.29], 1e-14);
%=============================================================================
% integrand written for scalar inputs
F = integralInterpolant(@(x) 2 * x - x ^ 2, 0, 1, 'Vectorized', false);
asserts.isapprox(F([0.5; 1]), [1 / 4 - 1 / 24; 2 / 3], 1e-12);
%=============================================================================
% infinite limits and endpoint singularity
F = integralInterpolant(@(x) exp(-x .^ 2), -Inf, Inf);
asserts.isapprox(F([-Inf, 0, Inf]), [0, sqrt(pi) / 2, sqrt(pi)], 1e-9);
F = integralInterpolant(@log, 0, 1);
asserts.isapprox(F([0, 0.5, 1]), [0, 0.5 * log(0.5) - 0.5, -1], 1e-7);
%=============================================================================
% display
F = integralInterpolant(@(x) x, 0, 1);
txt = evalc('disp(F)');
asserts.istrue(contains(txt, 'integralInterpolant with properties:'));
asserts.istrue(contains(txt, 'Subintervals: '));
%=============================================================================
asserts.checkerror('integralInterpolant(@(x) x, 1, 1)', _('Upper and lower limits must be different.'));
asserts.checkerror('integralInterpolant(@(x) x, 1, 1)', _('Upper and lower limits must be different.'), 'Nelson:integralInterpolant:equalLimits');
asserts.checkerror('integralInterpolant(@(x) x, 0, 1i)', _('Integration limits must be real numeric scalars.'), 'Nelson:integralInterpolant:invalidLimits');
asserts.checkerror('integralInterpolant(@(x) x, 0, [1 2])', _('Integration limits must be real numeric scalars.'), 'Nelson:integralInterpolant:invalidLimits');
asserts.checkerror('integralInterpolant(3, 0, 1)', _('First argument must be a function handle.'), 'Nelson:integralInterpolant:funArgNotHandle');
asserts.checkerror('integralInterpolant(@(x) x, 0, 1, ''Waypoints'', 1i)', _('''Waypoints'' value must be a vector with all finite real values.'), 'Nelson:integralInterpolant:invalidWaypoints');
asserts.checkerror('integralInterpolant(@(x) x, 0, 1, ''Foo'', 1)', sprintf(_('Unknown option name: %s'), 'Foo'), 'Nelson:integralInterpolant:unknownOption');
F = integralInterpolant(@(x) x, 0, 1);
asserts.checkerror('F(''a'')', _('Query points must be given as a single numeric array.'), 'Nelson:integralInterpolant:invalidQuery');
%=============================================================================
