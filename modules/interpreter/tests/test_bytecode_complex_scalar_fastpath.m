%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Parity checks for the bytecode VM complex-scalar binary arithmetic fast path.
% Values are hard-coded expected results (not a comparison against another
% engine): the fast path must match the documented general-path behaviour for
% +, -, *, /, == and ~=, including real narrowing and division by zero.
%=============================================================================
% Addition / subtraction
assert_isequal((1 + 2i) + (3 + 4i), 4 + 6i);
assert_isequal((1 + 2i) - (3 + 4i), -2 - 2i);
% mixed real + complex
assert_isequal(5 + (1 + 2i), 6 + 2i);
assert_isequal((1 + 2i) - 1, 2i);
%=============================================================================
% Multiplication
assert_isequal((1 + 2i) * (3 + 4i), -5 + 10i);
assert_isequal(2 * (3 + 4i), 6 + 8i);
assert_isequal((3 + 4i) * 2, 6 + 8i);
%=============================================================================
% Division
assert_isequal((1 + 1i) / (1 + 1i), 1);
assert_isequal((6 + 8i) / 2, 3 + 4i);
assert_isequal((4 + 2i) / (1 + 1i), 3 - 1i);
% left divide: a \ b == b / a
assert_isequal((1 + 1i) \ (4 + 2i), 3 - 1i);
assert_isequal(2 \ (6 + 8i), 3 + 4i);
%=============================================================================
% Real narrowing: an exactly-zero imaginary part yields a real double.
r1 = (1 + 2i) * (1 - 2i);
assert_isequal(r1, 5);
assert_istrue(isreal(r1));
assert_isequal(class(r1), 'double');

r2 = complex(2) * complex(3);
assert_isequal(r2, 6);
assert_istrue(isreal(r2));

r3 = (1 + 2i) + (1 - 2i);
assert_isequal(r3, 2);
assert_istrue(isreal(r3));

r4 = (2 + 4i) / 2;
assert_isequal(r4, 1 + 2i);
assert_isfalse(isreal(r4));
%=============================================================================
% complex(x) times a real narrows to a real double once imag is exactly zero.
c = complex(3);
assert_istrue(isreal(c * 2));
assert_isequal(c * 2, 6);
%=============================================================================
% Division by a zero complex denominator yields NaN + NaNi.
z = (1 + 1i) / (0 + 0i);
assert_istrue(isnan(real(z)));
assert_istrue(isnan(imag(z)));
%=============================================================================
% Equality / inequality compare both parts.
assert_istrue((1 + 2i) == (1 + 2i));
assert_isfalse((1 + 2i) == (1 + 3i));
assert_isfalse((1 + 2i) == (2 + 2i));
assert_istrue((1 + 2i) ~= (1 + 3i));
assert_isfalse((1 + 2i) ~= (1 + 2i));
% mixed real / complex equality
assert_istrue(3 == complex(3));
assert_isfalse(3 == (3 + 1i));
assert_istrue(3 ~= (3 + 1i));
%=============================================================================
% Same results inside a tight loop (exercises the hot bytecode path).
acc = 0 + 0i;
w = 0.5 + 0.25i;
for k = 1:1000
  acc = acc + w * (1 + 1i);
end
assert_isequal(acc, 1000 * (w * (1 + 1i)));
%=============================================================================
