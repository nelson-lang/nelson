%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
isNegZero = @(v) v == 0 & 1 ./ v < 0;
isPosZero = @(v) v == 0 & 1 ./ v > 0;
p2 = pi / 2;
%=============================================================================
for c = {'double', 'single'}
  tol = 1e-6;
  x = cast([-3 -1 -0.5 -0 0 0.5 1 3 Inf -Inf NaN], c{1});
  % atanh: outside [-1, 1] the imaginary part takes the sign of x
  R = atanh(x);
  assert_isequal(class(R), c{1});
  assert_isapprox(real(R), cast([atanh(-1/3) -Inf atanh(-0.5) 0 0 atanh(0.5) Inf atanh(1/3) 0 0 NaN], c{1}), tol);
  assert_isapprox(imag(R), cast([-p2 0 0 0 0 0 0 p2 p2 -p2 0], c{1}), tol);
  assert_isequal(isNegZero(real(R([4 10]))), [true true]);
  % acosh: Inf at +Inf, Inf + pi*i at -Inf, NaN keeps a zero imaginary part
  R = acosh(x);
  assert_isapprox(real(R), cast([acosh(3) 0 0 0 0 0 0 acosh(3) Inf Inf NaN], c{1}), tol);
  assert_isapprox(imag(R), cast([pi pi 2*pi/3 p2 p2 pi/3 0 0 0 pi 0], c{1}), tol);
  % asinh keeps -0
  R = asinh(x);
  assert_isequal(isreal(R), true);
  assert_isequal(isNegZero(R(4)), true);
  assert_isequal(R(9:11), cast([Inf -Inf NaN], c{1}));
  % asech, acsch, acoth through 1./x
  R = asech(cast([-0 0 0.5 2 NaN], c{1}));
  assert_isapprox(real(R), cast([Inf Inf acosh(2) 0 NaN], c{1}), tol);
  assert_isapprox(imag(R), cast([pi 0 0 pi/3 0], c{1}), tol);
  % on the real domain [0, 1], -0 gives NaN (scalar or array)
  assert_isequal(asech(cast(-0, c{1})), cast(NaN, c{1}));
  assert_isapprox(asech(cast([-0 0 0.5 NaN], c{1})), cast([NaN Inf acosh(2) NaN], c{1}), tol);
  assert_isequal(isNegZero(acsch(cast(-Inf, c{1}))), true);
  R = acoth(cast([-0.5 -0 0.5 -Inf], c{1}));
  assert_isapprox(real(R), cast([atanh(-0.5) 0 atanh(0.5) 0], c{1}), tol);
  assert_isapprox(imag(R), cast([-p2 -p2 p2 0], c{1}), tol);
  assert_isequal(isNegZero(real(R([2 4]))), [true true]);
end
%=============================================================================
% real results stay real
assert_isequal(isreal(atanh([-0.5 0 0.5 NaN])), true);
assert_isequal(isreal(acosh([1 2 Inf NaN])), true);
assert_isequal(acosh([1 Inf]), [0 Inf]);
%=============================================================================
% complex input
z = complex([0.5 2 -2 2 -Inf Inf -0 0], [1 0 0 -1 1 -Inf Inf -Inf]);
R = atanh(z);
assert_isapprox(R(1), complex(0.238877861256859, 0.847575660670829), 1e-12);
assert_isapprox(R(2:3), [complex(atanh(0.5), p2), complex(atanh(-0.5), -p2)], 1e-12);
assert_isapprox(R(4), complex(0.402359478108525, -1.338972522294493), 1e-12);
R = acosh(z);
assert_isapprox(R(5), complex(Inf, pi), 1e-12);
assert_isapprox(R(6), complex(Inf, -p2), 1e-12);
R = asinh(z);
assert_isapprox(R(7), complex(Inf, p2), 1e-12);
assert_isapprox(R(8), complex(-Inf, -p2), 1e-12);
R = atanh(complex([2 NaN 0], [NaN 1 NaN]));
assert_isequal(isnan(real(R)) & isnan(imag(R)), [true true true]);
%=============================================================================
% every element of a complex array is computed
z = complex(1:9, 1);
assert_isapprox(atanh(z), arrayfun(@(v) atanh(v), z), 1e-12);
%=============================================================================
% sparse input
S = sparse([0 -3 0.5]);
assert_isequal(issparse(atanh(S)), true);
assert_isapprox(full(atanh(S)), atanh([0 -3 0.5]), 1e-12);
assert_isapprox(full(asinh(S)), asinh([0 -3 0.5]), 1e-12);
assert_isapprox(full(acosh(S)), acosh([0 -3 0.5]), 1e-12);
%=============================================================================
