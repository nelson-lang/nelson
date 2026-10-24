%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The complex-by-real fast path for .* must stay bit-identical to the generic
% complex product (multiplying by the same value promoted to complex), for
% either operand order, and must keep the generic non-finite semantics by
% falling back.
%=============================================================================
for precision = {'double', 'single'}
  cast = precision{1};
  C = feval(cast, (cos(1:48) - 0.5) + 1i * (sin(1:48) - 0.5));
  C = reshape(C, 6, 8);
  r = feval(cast, reshape(cos((1:48) * 0.37) - 0.5, 6, 8));
  % matrix .* matrix, both orders
  assert_isequal(C .* r, C .* complex(r));
  assert_isequal(r .* C, complex(r) .* C);
  % matrix .* real scalar (positive, negative, zero)
  assert_isequal(C .* feval(cast, 3.75), C .* complex(feval(cast, 3.75)));
  assert_isequal(C .* feval(cast, -2.5), C .* complex(feval(cast, -2.5)));
  assert_isequal(C .* feval(cast, 0), C .* complex(feval(cast, 0)));
  % real scalar .* matrix
  assert_isequal(feval(cast, 1.7) .* C, complex(feval(cast, 1.7)) .* C);
  % complex scalar .* real matrix, both orders
  cs = feval(cast, 0.3 + 0.4i);
  assert_isequal(cs .* r, complex(cs) .* complex(r));
  assert_isequal(r .* cs, complex(r) .* complex(cs));
end
%=============================================================================
% Non-finite operands fall back to the generic path (identical results).
C = complex(cos(1:12), sin(1:12));
C = reshape(C, 3, 4);
r = reshape((1:12) * 0.1, 3, 4);
Ci = C; Ci(1) = complex(Inf, 1); Ci(2) = complex(1, Inf); Ci(3) = complex(NaN, 0);
assert_isequal(isequaln(Ci .* r, Ci .* complex(r)), true);
rn = r; rn(1) = Inf; rn(2) = NaN;
assert_isequal(isequaln(C .* rn, C .* complex(rn)), true);
%=============================================================================
% A purely real result (zero imaginary parts) collapses back to a real array.
z = complex(ones(2, 3), zeros(2, 3));
p = z .* 2;
assert_isequal(isreal(p), true);
assert_isequal(p, 2 * ones(2, 3));
%=============================================================================
