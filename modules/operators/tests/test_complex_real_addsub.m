%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The complex-by-real fast paths for + and - must stay bit-identical to the
% generic complex result (operating on the same value promoted to complex),
% respecting operand order for subtraction, and for every value (no cross term).
%=============================================================================
for precision = {'double', 'single'}
  cast = precision{1};
  C = feval(cast, (cos(1:48) - 0.5) + 1i * (sin(1:48) - 0.5));
  C = reshape(C, 6, 8);
  r = feval(cast, reshape(cos((1:48) * 0.37) - 0.5, 6, 8));
  rc = complex(r);
  % addition, both orders and scalars
  assert_isequal(C + r, C + rc);
  assert_isequal(r + C, rc + C);
  assert_isequal(C + feval(cast, 2.5), C + complex(feval(cast, 2.5)));
  assert_isequal(feval(cast, -1.3) + C, complex(feval(cast, -1.3)) + C);
  % subtraction, order sensitive
  assert_isequal(C - r, C - rc);
  assert_isequal(r - C, rc - C);
  assert_isequal(C - feval(cast, 2.5), C - complex(feval(cast, 2.5)));
  assert_isequal(feval(cast, 2.5) - C, complex(feval(cast, 2.5)) - C);
  % complex scalar with real matrix, both operators and orders
  cs = feval(cast, 0.3 + 0.4i);
  assert_isequal(cs + r, complex(cs) + rc);
  assert_isequal(cs - r, complex(cs) - rc);
  assert_isequal(r - cs, rc - complex(cs));
end
%=============================================================================
% Non-finite operands must still match the generic path (identical results).
C = complex(cos(1:12), sin(1:12));
C = reshape(C, 3, 4);
r = reshape((1:12) * 0.1, 3, 4);
rc = complex(r);
Ci = C; Ci(1) = complex(Inf, 1); Ci(2) = complex(1, -Inf); Ci(3) = complex(NaN, 2);
assert_isequal(isequaln(Ci - r, Ci - rc), true);
assert_isequal(isequaln(r - Ci, rc - Ci), true);
assert_isequal(isequaln(Ci + r, Ci + rc), true);
rn = r; rn(1) = Inf; rn(2) = NaN;
assert_isequal(isequaln(C - rn, C - complex(rn)), true);
%=============================================================================
% A purely real result collapses back to a real array.
z = complex(ones(2, 3), zeros(2, 3));
d = z - 1;
assert_isequal(isreal(d), true);
assert_isequal(d, zeros(2, 3));
%=============================================================================
