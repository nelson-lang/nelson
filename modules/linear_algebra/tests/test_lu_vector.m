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
A = [2 1 0; 4 3 1; 1 5 9];
L_REF = [1 0 0; 1/4 1 0; 1/2 -2/17 1];
U_REF = [4 3 1; 0 17/4 35/4; 0 0 9/17];
%=============================================================================
[L, U, p] = lu(A, 'vector');
asserts.isequal(p, [2 3 1]);
asserts.isequal(class(p), 'double');
asserts.isequal(size(p), [1 3]);
asserts.isapprox(L, L_REF, 1e-14);
asserts.isapprox(U, U_REF, 1e-14);
asserts.isapprox(L * U, A(p, :), 1e-14);
%=============================================================================
[L2, U2, P] = lu(A, 'matrix');
asserts.isequal(P, [0 1 0; 0 0 1; 1 0 0]);
asserts.isequal(L2, L);
asserts.isequal(U2, U);
[L3, U3, P3] = lu(A);
asserts.isequal(P3, P);
asserts.isequal(P(:, p), eye(3));
%=============================================================================
[L2, U2] = lu(A, 'vector');
[L0, U0] = lu(A);
asserts.isequal(L2, L0);
asserts.isequal(U2, U0);
asserts.isequal(lu(A, 'vector'), lu(A));
asserts.isequal(lu(A, 'matrix'), lu(A));
%=============================================================================
[~, ~, p] = lu(A, 'v');
asserts.isequal(p, [2 3 1]);
[~, ~, p] = lu(A, 'VeC');
asserts.isequal(p, [2 3 1]);
[~, ~, p] = lu(A, "vector");
asserts.isequal(p, [2 3 1]);
[~, ~, P] = lu(A, 'm');
asserts.isequal(size(P), [3 3]);
%=============================================================================
[L, U, p] = lu(single(A), 'vector');
asserts.isequal(class(L), 'single');
asserts.isequal(class(U), 'single');
asserts.isequal(class(p), 'double');
asserts.isequal(p, [2 3 1]);
asserts.isapprox(L, single(L_REF), 1e-6);
asserts.isapprox(U, single(U_REF), 1e-6);
[~, ~, P] = lu(single(A), 'matrix');
asserts.isequal(class(P), 'single');
%=============================================================================
[L, U, p] = lu(single(A) + 1i, 'vector');
asserts.isequal(class(p), 'double');
asserts.istrue(isreal(p));
asserts.isequal(p, [2 3 1]);
asserts.isapprox(L * U, single(A(p, :)) + 1i, 1e-5);
%=============================================================================
C = [1 2 3; 4 5 6] + 1i * [0 1 0; 2 0 1];
[L, U, p] = lu(C, 'vector');
asserts.isequal(p, [2 1]);
asserts.isequal(size(L), [2 2]);
asserts.isequal(size(U), [2 3]);
asserts.isapprox(L, [1 0; 0.2-0.1i 1], 1e-14);
asserts.isapprox(U, [4+2i 5 6+1i; 0 1+1.5i 1.7+0.4i], 1e-14);
asserts.isapprox(L * U, C(p, :), 1e-14);
%=============================================================================
B = [2 1; 4 3; 1 5; 0 7];
[L, U, p] = lu(B, 'vector');
asserts.isequal(p, [2 4 3 1]);
asserts.isequal(size(L), [4 2]);
asserts.isequal(size(U), [2 2]);
asserts.isapprox(L, [1 0; 0 1; 1/4 17/28; 1/2 -1/14], 1e-14);
asserts.isapprox(U, [4 3; 0 7], 1e-14);
[~, ~, p] = lu(single(B), 'vector');
asserts.isequal(p, [2 4 3 1]);
%=============================================================================
[L, U, p] = lu(sparse(B), 'vector');
asserts.isequal(p, [2 4 3 1]);
asserts.isfalse(issparse(p));
asserts.isequal(class(p), 'double');
asserts.isapprox(full(L * U), B(p, :), 1e-14);
%=============================================================================
R = rand(60, 40) + 1i * rand(60, 40);
[L, U, p] = lu(R, 'vector');
[~, ~, P] = lu(R);
asserts.isequal(P(:, p), eye(60));
asserts.isapprox(L * U, R(p, :), 1e-12);
R = rand(40, 60, 'single');
[L, U, p] = lu(R, 'vector');
[~, ~, P] = lu(R);
asserts.isequal(double(P(:, p)), eye(40));
asserts.isapprox(L * U, R(p, :), 1e-5);
%=============================================================================
[L, U, p] = lu(zeros(0, 3), 'vector');
asserts.isequal(size(L), [0 0]);
asserts.isequal(size(U), [0 3]);
asserts.isequal(size(p), [1 0]);
asserts.isequal(class(p), 'double');
[L, U, p] = lu(zeros(3, 0), 'vector');
asserts.isequal(size(L), [3 0]);
asserts.isequal(p, [1 2 3]);
[~, ~, p] = lu([], 'vector');
asserts.isequal(size(p), [1 0]);
[~, ~, p] = lu(single(zeros(2, 0)), 'vector');
asserts.isequal(p, [1 2]);
[~, ~, p] = lu(5, 'vector');
asserts.isequal(p, 1);
%=============================================================================
msg = _('The output form must be ''matrix'' or ''vector''.');
assert_checkerror('lu(A, ''foo'')', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(A, '''')', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(A, ''vector '')', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(A, [''v''; ''e''])', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(A, 3)', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(A, {''vector''})', msg, 'Nelson:lu:invalidOption');
assert_checkerror('lu(A, ''vector'', ''matrix'')', _('Too many input arguments.'));
assert_checkerror('[a, b, c, d] = lu(A, ''vector'')', _('Four or five outputs need a sparse matrix.'));
%=============================================================================
