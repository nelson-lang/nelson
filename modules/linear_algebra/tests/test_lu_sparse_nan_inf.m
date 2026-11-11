%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% partial pivoting: a NaN row wins, the lowest NaN row index first
[L, U] = lu(sparse([1 2; NaN 3]));
asserts.istrue(isequaln(full(L), [NaN 1; 1 0]));
asserts.istrue(isequaln(full(U), [NaN 3; 0 NaN]));
[L, U, p] = lu(sparse([1 2; NaN 3]), 'vector');
asserts.isequal(p, [2 1]);
asserts.istrue(isequaln(full(L), [1 0; NaN 1]));
asserts.istrue(isequaln(full(lu(sparse([1 2; NaN 3]))), [NaN 3; NaN NaN]));
[L, U] = lu(sparse([1 2; NaN 3]), 0);
asserts.istrue(isequaln(full(U), [NaN 3; 0 NaN]));
[L, U, p] = lu(sparse([NaN 2; 1 3]), 'vector');
asserts.isequal(p, [1 2]);
asserts.istrue(isequaln(full(U), [NaN 2; 0 NaN]));
[L, U] = lu(sparse(single([1 2; NaN 3])));
asserts.isequal(class(L), 'single');
asserts.istrue(isequaln(full(L), single([NaN 1; 1 0])));
asserts.istrue(isequaln(full(U), single([NaN 3; 0 NaN])));
%=============================================================================
[L, U, p] = lu(sparse([1 2 3; 4 5 6; 7 NaN 9]), 'vector');
asserts.isequal(p, [3 1 2]);
asserts.istrue(isequaln(full(L), [1 0 0; 1/7 1 0; 4/7 NaN 1]));
U = full(U);
asserts.isequal(isnan(U), logical([0 1 0; 0 1 0; 0 0 1]));
asserts.istrue(abs(U(2, 3) - (3 - 9 / 7)) < 1e-14);
asserts.isequal(U([1 2 3 6 7]), [7 0 0 0 9]);
%=============================================================================
[L, U] = lu(sparse([1 2; 3 4; NaN 6]));
asserts.istrue(isequaln(full(L), [NaN 1; NaN NaN; 1 0]));
asserts.istrue(isequaln(full(U), [NaN 6; 0 NaN]));
%=============================================================================
% sparsity pivoting: a NaN in the column makes every threshold test fail
[L, U, p, q] = lu(sparse([1 2; 3 4; NaN 6]), 'vector');
asserts.isequal(p, [1 3 2]);
asserts.isequal(q, [1 2]);
asserts.istrue(isequaln(full(L), [1 0; NaN 1; 3 NaN]));
asserts.istrue(isequaln(full(U), [1 2; 0 NaN]));
[L, U, p, q] = lu(sparse([0 1; NaN 0]), 'vector');
asserts.isequal(p, [2 1]);
asserts.isequal(q, [1 2]);
asserts.isequal(full(L), eye(2));
asserts.istrue(isequaln(full(U), [NaN 0; 0 1]));
[L, U, P, Q] = lu(sparse([1 2 3; NaN 5 6; 7 8 9]), [1 1]);
asserts.isequal(full(P), eye(3));
asserts.isequal(full(Q), eye(3));
asserts.istrue(isequaln(full(L), [1 0 0; NaN 1 0; 7 NaN 1]));
asserts.istrue(isequaln(full(U), [1 2 3; 0 NaN NaN; 0 0 NaN]));
[L, U, P, Q] = lu(sparse([1 2; Inf 3]), 0);
asserts.isequal(full(P), eye(2));
asserts.isequal(full(L), [1 0; Inf 1]);
asserts.isequal(full(U), [1 2; 0 -Inf]);
%=============================================================================
% row scaling: a NaN row sum scales by 1, an Inf row sum by Inf
[L, U, p, q, R] = lu(sparse([1 2; NaN 3]), 'vector');
asserts.isequal(full(R), [3 0; 0 1]);
asserts.istrue(isequaln(full(L), [1 0; NaN 1]));
asserts.istrue(isequaln(full(U), [1/3 2/3; 0 NaN]));
[L, U, p, q, R] = lu(sparse([Inf 2; 1 3]), 'vector');
asserts.isequal(full(R), [Inf 0; 0 4]);
asserts.istrue(isequaln(full(L), [1 0; NaN 1]));
asserts.istrue(isequaln(full(U), [NaN 0; 0 NaN]));
%=============================================================================
% a real complex pivot divides each part on its own
[L, U, p, q] = lu(sparse([1 2i; NaN + 1i 3]), 'vector');
asserts.isequal(p, [1 2]);
asserts.istrue(isnan(real(full(L(2, 1)))));
asserts.isequal(imag(full(L(2, 1))), 1);
asserts.isequal(full(U(1, :)), [1 2i]);
asserts.istrue(isnan(real(full(U(2, 2)))) && isnan(imag(full(U(2, 2)))));
[L, U, p, q] = lu(sparse([1 2; complex(0, NaN) 3]), 'vector');
asserts.isequal(real(full(L(2, 1))), 0);
asserts.istrue(isnan(imag(full(L(2, 1)))));
%=============================================================================
