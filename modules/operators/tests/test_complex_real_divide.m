%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The complex-numerator / real-divisor fast path must stay bit-identical to the
% generic complex quotient (dividing by the same value promoted to complex), and
% must keep the generic non-finite / zero-divisor semantics by falling back.
%=============================================================================
% The accurate complex/real scaling fast path (a/c + (b/c)i) agrees with the
% generic complex quotient only to within one unit in the last place: the
% generic path promotes the divisor to complex and, depending on the platform
% BLAS / Eigen complex-division kernel, evaluates (a*c)/(c^2) instead of a/c.
% Compare with a tight relative tolerance (per precision) rather than demanding
% bit-identity, which does not hold across every backend.
for precision = {'double', 'single'}
  cast = precision{1};
  if strcmp(cast, 'single')
    tol = 1e-5;
  else
    tol = 1e-12;
  end
  C = feval(cast, (cos(1:48) - 0.5) + 1i * (sin(1:48) - 0.5));
  C = reshape(C, 6, 8);
  r = feval(cast, reshape(abs(cos((1:48) * 0.37)) + 0.1, 6, 8));
  % matrix ./ matrix
  assert_isapprox(C ./ r, C ./ complex(r), tol);
  % matrix ./ real scalar (positive and negative)
  assert_isapprox(C ./ feval(cast, 3.75), C ./ complex(feval(cast, 3.75)), tol);
  assert_isapprox(C ./ feval(cast, -2.5), C ./ complex(feval(cast, -2.5)), tol);
  % matrix ./ negative matrix
  assert_isapprox(C ./ (-r), C ./ complex(-r), tol);
  % scalar complex ./ real matrix
  z = feval(cast, 0.3 + 0.7i);
  assert_isapprox(z ./ r, z ./ complex(r), tol);
  % symmetric left divide: r .\ C == C ./ r (both take the same fast path)
  assert_isequal(r .\ C, C ./ r);
  assert_isapprox(feval(cast, 2) .\ C, C ./ complex(feval(cast, 2)), tol);
  % N-dimensional
  C3 = feval(cast, reshape((1:24) + 1i * (24:-1:1), 2, 3, 4));
  r3 = feval(cast, reshape((1:24) * 0.5 + 0.1, 2, 3, 4));
  assert_isapprox(C3 ./ r3, C3 ./ complex(r3), tol);
end
%=============================================================================
% Exact values.
R = [2 + 4i, 6 + 8i] ./ [2, 4];
assert_isequal(R, [1 + 2i, 1.5 + 2i]);
assert_isequal(single([2 + 4i, 6 + 8i]) ./ single([2, 4]), single([1 + 2i, 1.5 + 2i]));
%=============================================================================
% A complex-typed but all-real numerator divided by a real operand demotes.
res = complex([1 2 3], [0 0 0]) ./ 2;
assert_isequal(res, [0.5 1 1.5]);
assert_istrue(isreal(res));
res = complex(single([4 8]), single([0 0])) ./ single(4);
assert_istrue(isreal(res));
%=============================================================================
% Zero / non-finite divisors keep the generic semantics (fall back).
assert_istrue(all(isnan(real(complex([1 2]) ./ 0))));
assert_istrue(all(isnan(imag(complex([1 2]) ./ 0))));
assert_istrue(all(isnan(real((1 + 1i) ./ [0 -0]))));
assert_isequal((3 + 4i) ./ Inf, complex(3 + 4i) ./ complex(Inf));
%=============================================================================
% Empty operands.
assert_isequal(complex(zeros(0, 3)) ./ zeros(0, 3), complex(zeros(0, 3)) ./ complex(zeros(0, 3)));
%=============================================================================
