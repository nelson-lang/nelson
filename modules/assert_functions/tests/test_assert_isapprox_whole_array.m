%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% assert_isapprox uses the whole-array rule of isapproxnorm
asserts.istrue(assert_isapprox(1, 1));
asserts.isfalse(assert_isapprox(pi, single(pi)));
asserts.istrue(assert_isapprox(pi, single(pi), 1e-4));
asserts.isfalse(assert_isapprox(1, [2 3], eps));
asserts.istrue(assert_isapprox([], []));
asserts.isfalse(assert_isapprox([], zeros(0, 3)));
asserts.istrue(assert_isapprox(ones(10, 1) + eps, ones(10, 1), eps));
asserts.isfalse(assert_isapprox(zeros(10, 1) + 1.e-4, zeros(10, 1), 1.e-5));
asserts.istrue(assert_isapprox(1.23456789123456789e-30, 1.23456789123456789e-30, eps));
asserts.isfalse(assert_isapprox(1.23456789123456789e-30, 1.3e-30, eps));
asserts.isfalse(assert_isapprox(1 + 5 * eps, 1, eps));
asserts.istrue(assert_isapprox(1 + 5 * eps, 1, 10 * eps));
%=============================================================================
% one large element absorbs a small relative error elsewhere
asserts.istrue(assert_isapprox([1 1.e5], [2 1.e5], 1.e-3));
%=============================================================================
% special values must be at the same places
asserts.istrue(assert_isapprox([1 NaN 1 Inf], [1 NaN 1 Inf], eps));
asserts.istrue(assert_isapprox([1.2345 Inf -Inf NaN], [1.2346 Inf -Inf NaN], 1.e-4));
asserts.isfalse(assert_isapprox([NaN 1], [1 NaN], eps));
asserts.isfalse(assert_isapprox([1 Inf], [2 Inf], 1.e-3));
asserts.isfalse(assert_isapprox([1 Inf -Inf NaN], [1 -Inf Inf NaN], 1.e-3));
%=============================================================================
% complex values: real and imaginary parts are checked separately
asserts.istrue(assert_isapprox(1 + i, 1 + (1 + 1.e-4) * i, 1.e-3));
asserts.isfalse(assert_isapprox(1 + i, 1 + (1 + 1.e-4) * i, 1.e-5));
%=============================================================================
% sparse values need the same pattern
asserts.istrue(assert_isapprox(sparse(eye(3, 3) + eps), sparse(eye(3, 3) + 2 * eps), 10 * eps));
asserts.istrue(assert_isapprox(sparse([1 0 2]), [1 0 2]));
asserts.isfalse(assert_isapprox(sparse([1 0 2]), sparse([1 1e-30 2]), 1e-3));
%=============================================================================
asserts.istrue(assert_isapprox(int8(3), 3));
asserts.istrue(assert_isapprox(int8(3), int16(3)));
A = rand(3, 3, 3);
asserts.istrue(assert_isapprox(A, A));
asserts.checkerror('assert_isapprox(1, ''A'', 0.1, 0)', _('Numerics types expected.'));
%=============================================================================
% a class that overloads isapproxnorm is compared with its own method
d = [tempname(), '_approx'];
mkdir(d);
lines = {'classdef approxTestValue', '  properties', '    v', '  end', '  methods', ...
  '    function obj = approxTestValue(v)', '      obj.v = v;', '    end', ...
  '    function tf = isapproxnorm(a, b, varargin)', '      tf = abs(a.v - b.v) < 1;', '    end', ...
  '  end', 'end'};
filewrite([d, '/approxTestValue.m'], strjoin(lines, newline));
addpath(d);
asserts.istrue(assert_isapprox(approxTestValue(1), approxTestValue(1.5)));
asserts.isfalse(assert_isapprox(approxTestValue(1), approxTestValue(3)));
rmpath(d);
rmdir(d, 's');
%=============================================================================
% an isapproxnorm overload of a builtin type is also used
d = [tempname(), '_approx_double'];
mkdir([d, '/@double']);
lines = {'function tf = isapproxnorm(a, b, p)', '  tf = p == 0.5 & abs(a - b) < 1000;', 'end'};
filewrite([d, '/@double/isapproxnorm.m'], strjoin(lines, newline));
addpath(d);
asserts.istrue(assert_isapprox(1, 100, 0.5));
asserts.isfalse(assert_isapprox(1, 5000, 0.5));
rmpath(d);
rmdir(d, 's');
asserts.isfalse(assert_isapprox(1, 100, 0.5));
%=============================================================================
