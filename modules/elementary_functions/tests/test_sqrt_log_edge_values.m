%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
isPosZero = @(v) v == 0 & 1 ./ v > 0;
%=============================================================================
for c = {'double', 'single'}
  x = cast([-4 -0 0 0.25 Inf -Inf NaN], c{1});
  % sqrt: one negative value makes the whole result complex
  R = sqrt(x);
  assert_isequal(class(R), c{1});
  assert_isequal(isreal(R), false);
  assert_isequal(real(R), cast([0 0 0 0.5 Inf 0 NaN], c{1}));
  assert_isequal(imag(R), cast([2 0 0 0 0 Inf 0], c{1}));
  assert_isequal(isPosZero(real(R(2:3))), [true true]);
  % log, log2, log10: -0 stays on the real axis, NaN keeps a zero imaginary part
  R = log(x);
  assert_isapprox(real(R), cast([log(4) -Inf -Inf log(0.25) Inf Inf NaN], c{1}), 1e-6);
  assert_isapprox(imag(R), cast([pi 0 0 0 0 pi 0], c{1}), 1e-6);
  R = log2(x);
  assert_isequal(real(R), cast([2 -Inf -Inf -2 Inf Inf NaN], c{1}));
  assert_isapprox(imag(R), cast([pi 0 0 0 0 pi 0] / log(2), c{1}), 1e-6);
  R = log10(x);
  assert_isapprox(real(R), cast([log10(4) -Inf -Inf log10(0.25) Inf Inf NaN], c{1}), 1e-6);
  assert_isapprox(imag(R), cast([pi 0 0 0 0 pi 0] / log(10), c{1}), 1e-6);
end
%=============================================================================
% no negative value: real results, sqrt(-0) is +0
assert_isequal(isreal(sqrt([4 -0 NaN])), true);
assert_isequal(isPosZero(sqrt([-0 4])), [true false]);
assert_isequal(log2([1 8 0.5 -0]), [0 3 -1 -Inf]);
assert_isequal(isreal(log10([1 100 -0])), true);
%=============================================================================
% large and tiny values neither overflow nor vanish in a complex result
R = sqrt([-1 1e300 1e-300]);
assert_isequal(real(R), [0 1e150 1e-150]);
assert_isequal(imag(R), [1 0 0]);
%=============================================================================
% complex input: zero imaginary parts follow the real rules, other NaN give NaN + NaNi
z = complex([-2 Inf -Inf NaN 1 -Inf], [0 1 1 0 NaN NaN]);
R = sqrt(z);
assert_isapprox(real(R), [0 Inf 0 NaN NaN NaN], 1e-12);
assert_isapprox(imag(R), [sqrt(2) 0 Inf 0 NaN NaN], 1e-12);
R = log(complex([-2 -0 NaN 1], [0 0 0 NaN]));
assert_isapprox(real(R), [log(2) -Inf NaN NaN], 1e-12);
assert_isequal(imag(R), [pi 0 0 NaN]);
%=============================================================================
% single complex log10 (used to crash)
R = log10(complex(single([10 100]), single([0 1])));
assert_isequal(class(R), 'single');
assert_isapprox(double(R), [1, log10(complex(100, 1))], 1e-6);
%=============================================================================
