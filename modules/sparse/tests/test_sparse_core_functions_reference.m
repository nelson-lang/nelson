%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse([4 0 1; 0 5 0; 2 0 6]);

R = min(A);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([0 0 0]));
asserts.isequal(nzmax(R), 1);

R = max(A, [], 2);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([4; 5; 6]));

R = cumsum(A);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([4 0 1; 4 5 1; 6 5 7]));

R = cumsum(A, 2);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([4 4 5; 0 5 5; 2 2 8]));

[B, d] = spdiags(A);
asserts.isequal(B, [2 4 0; 0 5 0; 0 6 1]);
asserts.isequal(d, [-2; 0; 2]);

B = [10 20; 30 40; 50 60; 70 80];
R = spdiags(B, [-1 1], 4, 4);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [0 40 0 0; 10 0 60 0; 0 30 0 80; 0 0 50 0]);

R = spdiags([10; 20; 0], 0, A);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [10 0 1; 0 20 0; 2 0 0]);

R = spdiags(B, [-1 1], sparse(ones(4, 4)));
asserts.istrue(issparse(R));
asserts.isequal(full(R), [1 40 1 1; 10 1 60 1; 1 30 1 80; 1 1 50 1]);

T = sparse([0 1 1; 1 0 0; 1 0 0]);
R = spdiags(-sum(T, 2) + 1, 0, double(T));
asserts.istrue(issparse(R));
asserts.isequal(full(R), [-1 1 1; 1 0 0; 1 0 0]);

R = spalloc(3, 4, 5);
asserts.istrue(issparse(R));
asserts.isequal(size(R), [3 4]);
asserts.isequal(nnz(R), 0);
asserts.isequal(nzmax(R), 5);

D = [1 1 10; 2 3 20; 3 2 30];
R = spconvert(D);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [10 0 0; 0 0 20; 0 30 0]);

R = diag(A);
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([4; 5; 6]));

R = diag(sparse([1 2 3]));
asserts.istrue(issparse(R));
asserts.isequal(R, sparse([1 0 0; 0 2 0; 0 0 3]));

[I, J, V] = find(A);
asserts.isequal(I, [1; 3; 2; 1; 3]);
asserts.isequal(J, [1; 1; 2; 3; 3]);
asserts.isequal(V, [4; 2; 5; 1; 6]);

asserts.isequal(norm(A, 1), 7);
asserts.isequal(norm(A, inf), 8);
asserts.isapprox(norm(A, 'fro'), 9.0553851381374173, 1e-12);

R = repmat(A, 2, 2);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [4 0 1 4 0 1; 0 5 0 0 5 0; 2 0 6 2 0 6; ...
                         4 0 1 4 0 1; 0 5 0 0 5 0; 2 0 6 2 0 6]);

[L, U, P] = lu(A);
asserts.istrue(issparse(L));
asserts.istrue(issparse(U));
asserts.istrue(issparse(P));
asserts.isapprox(full(P * A), full(L * U), 1e-12);

A = sparse([0 2 0; 3 0 4; 5 6 0]);
[L, U] = lu(A);
asserts.istrue(issparse(L));
asserts.istrue(issparse(U));
asserts.isapprox(full(L), [0 -0.5555555555555556 1; 0.6 1 0; 1 0 0], 1e-12);
asserts.isapprox(full(U), [5 6 0; 0 -3.6 4; 0 0 2.2222222222222223], 1e-12);

[L, U, P] = lu(A);
asserts.istrue(issparse(L));
asserts.istrue(issparse(U));
asserts.istrue(issparse(P));
asserts.isapprox(full(L), [1 0 0; 0.6 1 0; 0 -0.5555555555555556 1], 1e-12);
asserts.isapprox(full(U), [5 6 0; 0 -3.6 4; 0 0 2.2222222222222223], 1e-12);
asserts.isequal(full(P), [0 0 1; 0 1 0; 1 0 0]);
asserts.isapprox(full(P * A), full(L * U), 1e-12);

C = sparse([4 1 0; 1 3 0; 0 0 2]);
[R, p] = chol(C);
asserts.istrue(issparse(R));
asserts.isequal(p, 0);
asserts.isapprox(full(R' * R), full(C), 1e-12);
%=============================================================================
