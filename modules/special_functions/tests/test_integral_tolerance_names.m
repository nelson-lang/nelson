%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% documented example: singularity at the lower limit
fun = @(x) log(x);
q1 = integral(fun, 0, 1);
asserts.isapprox(q1, -1, 1e-7);
q2 = integral(fun, 0, 1, 'AbsoluteTolerance', 1e-12, 'RelativeTolerance', 0);
asserts.isapprox(q2, -1, 0, 1e-11);
asserts.istrue(abs(q2 + 1) < abs(q1 + 1));
%=============================================================================
% documented example: improper integral of an oscillatory function
fun = @(x) x .^ 5 .* exp(-x) .* sin(x);
q = integral(fun, 0, Inf, 'AbsoluteTolerance', 1e-13, 'RelativeTolerance', 1e-8);
asserts.isapprox(q, -15, 1e-8);
%=============================================================================
% the former names are still accepted
q = integral(@(x) exp(-x .^ 2), 0, Inf, 'RelTol', 1e-12, 'AbsTol', 1e-14);
asserts.isapprox(q, sqrt(pi) / 2, 1e-11);
q = integral(@(x) exp(-x .^ 2), 0, Inf, 'relativetolerance', 1e-12, 'absolutetolerance', 1e-14);
asserts.isapprox(q, sqrt(pi) / 2, 1e-11);
%=============================================================================
% documented example of integral2
a = 3;
b = 5;
fun = @(x, y) a * x .^ 2 + b * y .^ 2;
q = integral2(fun, 0, 5, -5, 0, 'AbsoluteTolerance', 0, 'RelativeTolerance', 1e-10);
asserts.isapprox(q, 5000 / 3, 1e-10);
%=============================================================================
q = integral3(@(x, y, z) x .* y .* z, 0, 1, 0, 1, 0, 1, 'AbsoluteTolerance', 0, 'RelativeTolerance', 1e-10);
asserts.isapprox(q, 1 / 8, 1e-10);
%=============================================================================
msg = sprintf(_('%s must be a nonnegative real scalar.'), 'AbsoluteTolerance');
asserts.checkerror('integral(@(x) x, 0, 1, ''AbsoluteTolerance'', -1)', msg);
asserts.checkerror('integral(@(x) x, 0, 1, ''AbsoluteTolerance'', -1)', msg, 'Nelson:integral:AbsoluteTolerance');
msg = sprintf(_('%s must be a nonnegative real scalar.'), 'RelativeTolerance');
asserts.checkerror('integral2(@(x, y) x, 0, 1, 0, 1, ''RelTol'', [1 2])', msg, 'Nelson:integral2:RelativeTolerance');
asserts.checkerror('integral3(@(x, y, z) x, 0, 1, 0, 1, 0, 1, ''RelativeTolerance'', -1)', msg, 'Nelson:integral3:RelativeTolerance');
asserts.checkerror('integral3(@(x, y, z) x, 0, 1, 0, 1, 0, 1, ''Method'', ''foo'')', _('Method must be ''auto'', ''tiled'', or ''iterated''.'), 'Nelson:integral3:Method');
asserts.checkerror('integral2(@(x, y) x, 0, 1, 0, 1, ''ArrayValued'', true)', sprintf(_('Unknown option name: %s'), 'ArrayValued'), 'Nelson:integral2:unknownOption');
%=============================================================================
