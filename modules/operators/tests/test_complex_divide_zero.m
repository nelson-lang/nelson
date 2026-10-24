%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Dense Eigen quotient kernels retain their native complex-zero results.
% Sparse and broadcast scalar kernels keep their existing nonfinite semantics.
for precision = {'double', 'single'}
  numerators = feval(precision{1}, [0, 1, -1, 1i, 1 + 2i, Inf, NaN, complex(Inf, Inf)]);
  divisors = feval(precision{1}, [0, -0, 1i]);
  result = feval(precision{1}, 1) ./ divisors;
  check_complex_nan(result(1:2));
  assert_isequal(result(3), feval(precision{1}, -1i));

  check_complex_nan(numerators ./ feval(precision{1}, 0));
  check_complex_nan(numerators ./ feval(precision{1}, -0));
  check_complex_nan(numerators ./ zeros(size(numerators), precision{1}));
  check_complex_nan(zeros(size(numerators), precision{1}) .\ numerators);

  values = feval(precision{1}, [1 + 1i, 2i]);
  result = values ./ divisors.';
  assert_isequal(real(result(1, 1)), feval(precision{1}, Inf));
  assert_isequal(imag(result(1, 2)), feval(precision{1}, Inf));
  result = reshape(values, 1, 1, 2) ./ reshape(divisors, 1, 3, 1);
  assert_isequal(real(result(1, 1, 1)), feval(precision{1}, Inf));
  assert_isequal(imag(result(1, 1, 2)), feval(precision{1}, Inf));

  result = sparse(values) ./ feval(precision{1}, 0);
  assert_istrue(issparse(result));
  assert_isequal(real(full(result(1))), feval(precision{1}, Inf));
  assert_isequal(imag(full(result(2))), feval(precision{1}, Inf));
  result = values ./ sparse(zeros(size(values), precision{1}));
  assert_isequal(real(full(result(1))), feval(precision{1}, Inf));
  assert_isequal(imag(full(result(2))), feval(precision{1}, Inf));
end
assert_isequal(1 ./ 0, Inf);
assert_isequal(-1 ./ 0, -Inf);
assert_isequal(0 ./ 0, NaN);
%=============================================================================
function check_complex_nan(value)
  assert_istrue(all(isnan(real(value)), 'all'));
  assert_istrue(all(isnan(imag(value)), 'all'));
end
%=============================================================================
