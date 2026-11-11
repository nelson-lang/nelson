%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isapproxnorm'), -2);
asserts.isequal(nargout('isapproxnorm'), 1);
%=============================================================================
% one logical scalar for the whole array
R = isapproxnorm(1, 1);
asserts.isequal(R, true);
asserts.isequal(isapproxnorm(ones(3), ones(3)), true);
asserts.isfalse(isapproxnorm(1, 1 + eps));
asserts.istrue(isapproxnorm(1, 1 + eps, eps));
asserts.istrue(isapproxnorm(ones(10, 1) + eps, ones(10, 1), eps));
asserts.isfalse(isapproxnorm(zeros(10, 1) + 1.e-4, zeros(10, 1), 1.e-5));
asserts.istrue(isapproxnorm(1.23456789123456789e-30, 1.23456789123456789e-30, eps));
asserts.isfalse(isapproxnorm(1.23456789123456789e-30, 1.3e-30, eps));
asserts.istrue(isapproxnorm(1e10, 1e10 + 1, 1e-8));
%=============================================================================
% one large element absorbs a small relative error elsewhere
asserts.istrue(isapproxnorm([1 1.e5], [2 1.e5], 1.e-3));
asserts.isfalse(isapproxnorm([1 1.e5], [2 1.e5], 1.e-6));
%=============================================================================
% dimensions must match, no implicit expansion
asserts.isfalse(isapproxnorm(1, [1 1]));
asserts.isfalse(isapproxnorm([1; 2], [1 2], 1));
asserts.istrue(isapproxnorm([], []));
asserts.isfalse(isapproxnorm([], zeros(0, 3)));
asserts.istrue(isapproxnorm(zeros(0, 3), zeros(0, 3)));
%=============================================================================
% special values must be at the same places and are left out of the norms
asserts.istrue(isapproxnorm([1 NaN 1 Inf], [1 NaN 1 Inf], eps));
asserts.istrue(isapproxnorm([1.2345 Inf -Inf NaN], [1.2346 Inf -Inf NaN], 1.e-4));
asserts.isfalse(isapproxnorm([NaN 1], [1 NaN], eps));
asserts.isfalse(isapproxnorm([1 Inf], [2 Inf], 1.e-3));
asserts.isfalse(isapproxnorm([1 Inf -Inf NaN], [1 -Inf Inf NaN], 1.e-3));
asserts.istrue(isapproxnorm(NaN, NaN));
%=============================================================================
% complex values: real and imaginary parts are checked separately
asserts.istrue(isapproxnorm(1 + i, 1 + (1 + 1.e-4) * i, 1.e-3));
asserts.isfalse(isapproxnorm(1 + i, 1 + (1 + 1.e-4) * i, 1.e-5));
asserts.istrue(isapproxnorm(complex(1, NaN), complex(1, NaN)));
asserts.isfalse(isapproxnorm(complex(1, NaN), complex(NaN, 1)));
%=============================================================================
% sparse values need the same pattern of nonzero values
asserts.istrue(isapproxnorm(sparse(eye(3, 3) + eps), sparse(eye(3, 3) + 2 * eps), 10 * eps));
asserts.istrue(isapproxnorm(sparse([1 0 2]), [1 0 2]));
asserts.isfalse(isapproxnorm(sparse([1 0 2]), sparse([1 1e-30 2]), 1e-3));
asserts.istrue(isapproxnorm(sparse([1 0 NaN]), sparse([1 0 NaN])));
asserts.istrue(isapproxnorm(sparse([1i 0 2]), sparse([1i 0 2])));
asserts.isfalse(isapproxnorm(sparse([1i 0 2]), sparse([0 1i 2])));
%=============================================================================
% values are compared in double precision
asserts.isfalse(isapproxnorm(pi, single(pi)));
asserts.istrue(isapproxnorm(pi, single(pi), 1e-4));
asserts.istrue(isapproxnorm(int8(3), 3));
asserts.istrue(isapproxnorm(int8(3), int16(3)));
A = rand(3, 3, 3);
asserts.istrue(isapproxnorm(A, A));
%=============================================================================
% long arrays use the fast sums, special values and overflow use the exact path
X = rand(1, 1001);
asserts.istrue(isapproxnorm(X, X + 1e-12, 1e-8));
asserts.isfalse(isapproxnorm(X, X + 1e-6, 1e-8));
Y = X; Y(500) = NaN; Z = Y; Z(1) = Z(1) + 1e-12;
asserts.istrue(isapproxnorm(Y, Z, 1e-8));
Z(500) = 1;
asserts.isfalse(isapproxnorm(Y, Z, 1e-8));
asserts.istrue(isapproxnorm([1e200 1], [1e200 1], eps));
%=============================================================================
% huge values are scaled so that the squares do not overflow
asserts.istrue(isapproxnorm([1e200 1], [1e200 1]));
asserts.istrue(isapproxnorm([1e300 1e300], [1e300 1.0001e300], 1e-3));
asserts.isfalse(isapproxnorm([1e300 1e300], [1e300 1.1e300], 1e-3));
asserts.istrue(isapproxnorm(1e-200, 1.01e-200, 0.1));
asserts.istrue(isapproxnorm([1e300 NaN], [1.0001e300 NaN], 1e-3));
asserts.isfalse(isapproxnorm(complex(1, 1e300), complex(1, 2e300), 0.1));
asserts.istrue(isapproxnorm(sparse([1e300 0 1e300]), sparse([1e300 0 1e300])));
asserts.isfalse(isapproxnorm(sparse([1e300 0]), sparse([2e300 0]), 0.1));
asserts.istrue(isapproxnorm(complex(X, X), complex(X, X + 1e-12), 1e-8));
asserts.isfalse(isapproxnorm(complex(X, X), complex(X, X + 1e-6), 1e-8));
W = complex(X, Y);
asserts.istrue(isapproxnorm(W, W, 0));
%=============================================================================
msg = _('Precision must be a nonnegative finite real scalar.');
asserts.checkerror('isapproxnorm(1, 1, -1)', msg);
asserts.checkerror('isapproxnorm(1, 1, NaN)', msg);
asserts.checkerror('isapproxnorm(1, 1, Inf)', msg);
asserts.checkerror('isapproxnorm(1, 1, [1 2])', msg);
asserts.checkerror('isapproxnorm(1, 1, 1i)', msg);
asserts.checkerror('isapproxnorm(1, 1, ''a'')', msg);
asserts.checkerror('isapproxnorm(1, 1, true)', msg);
asserts.istrue(isapproxnorm(1, 1, single(0.1)));
asserts.istrue(isapproxnorm(1, 1, int8(1)));
%=============================================================================
msg = _('Numerics types expected.');
asserts.checkerror('isapproxnorm(1, ''A'')', msg);
asserts.checkerror('isapproxnorm(true, true)', msg);
asserts.checkerror('isapproxnorm(1)', _('Not enough input arguments.'));
asserts.checkerror('isapproxnorm(1, 2, 3, 4)', _('Too many input arguments.'));
%=============================================================================
