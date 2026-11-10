%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A zero divisor divides each part on its own; an exact zero part paired with a
% nonzero one stays 0. Dense, broadcast, sparse and scalar paths agree.
for precision = {'double', 'single'}
  c = precision{1};
  numerators = feval(c, [0, 1, -1, 1i, 1 + 2i, Inf, NaN, complex(Inf, Inf)]);
  divisors = feval(c, [0, -0, 1i]);
  result = feval(c, 1) ./ divisors;
  assert_isequal(real(result), feval(c, [Inf -Inf 0]));
  assert_isequal(imag(result), feval(c, [0 0 -1]));

  expectedReal = feval(c, [NaN Inf -Inf 0 Inf Inf NaN Inf]);
  expectedImag = feval(c, [0 0 0 Inf Inf 0 0 Inf]);
  for R = {numerators ./ feval(c, 0), numerators ./ zeros(size(numerators), c), ...
      zeros(size(numerators), c) .\ numerators}
    assert_isequal(real(R{1}), expectedReal);
    assert_isequal(imag(R{1}), expectedImag);
  end
  R = numerators ./ feval(c, -0);
  assert_isequal(real(R), -expectedReal);
  assert_isequal(imag(R), -expectedImag);

  values = feval(c, [1 + 1i, 2i]);
  result = values ./ divisors.';
  assert_isequal(real(result), feval(c, [Inf 0; -Inf 0; 1 2]));
  assert_isequal(imag(result), feval(c, [Inf Inf; -Inf -Inf; -1 0]));
  result = reshape(values, 1, 1, 2) ./ reshape(divisors, 1, 3, 1);
  assert_isequal(real(result(1, 1, 1)), feval(c, Inf));
  assert_isequal(imag(result(1, 1, 2)), feval(c, Inf));
  assert_isequal(real(result(1, 1, 2)), feval(c, 0));

  result = sparse(values) ./ feval(c, 0);
  assert_istrue(issparse(result));
  assert_isequal(real(full(result)), feval(c, [Inf 0]));
  assert_isequal(imag(full(result)), feval(c, [Inf Inf]));
  result = values ./ sparse(zeros(size(values), c));
  assert_isequal(real(full(result)), feval(c, [Inf 0]));
  assert_isequal(imag(full(result)), feval(c, [Inf Inf]));
end
%=============================================================================
% scalar forms
z = (1 + 1i) / (0 + 0i);
assert_isequal([real(z), imag(z)], [Inf Inf]);
z = (1 + 1i) / complex(0, 0);
assert_isequal([real(z), imag(z)], [Inf Inf]);
assert_isequal(1 ./ 0, Inf);
assert_isequal(-1 ./ 0, -Inf);
assert_isequal(0 ./ 0, NaN);
%=============================================================================
% infinite divisors of finite values give 0
R = (3 + 4i) ./ [Inf, complex(Inf, 1), complex(-Inf, Inf)];
assert_isequal(abs(R), [0 0 0]);
%=============================================================================
