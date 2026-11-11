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
% a complex diagonal entry fails like a non positive pivot
A = [2 1; 1 3+1i];
assert_checkerror('chol(A)', _('Positive definite matrix with a real diagonal expected.'), 'Nelson:chol:realDiagonalExpected');
[R, p] = chol(A);
asserts.isequal(p, 2);
asserts.isequal(R, sqrt(2));
[R, p] = chol(A, 'lower');
asserts.isequal(p, 2);
asserts.isequal(R, sqrt(2));
%=============================================================================
[R, p] = chol([-1 0; 0 1+1i]);
asserts.isequal(p, 1);
asserts.isequal(R, []);
%=============================================================================
[R, p] = chol([2 0 0; 0 3 0; 0 0 1+1e-300i]);
asserts.isequal(p, 3);
asserts.isequal(R, [sqrt(2) 0; 0 sqrt(3)]);
%=============================================================================
[R, p] = chol([4 2 0; 2 5 1; 0 1 6+2i]);
asserts.isequal(p, 3);
asserts.isequal(R, [2 1; 0 2]);
%=============================================================================
% NaN imaginary part on the diagonal
[R, p] = chol([1 0; 0 complex(2, NaN)]);
asserts.isequal(p, 2);
asserts.isequal(R, 1);
%=============================================================================
% the first failure wins, the message still names the real diagonal
A = [4 2 1; 2 -5 1; 1 1 3+1i];
[R, p] = chol(A);
asserts.isequal(p, 2);
asserts.isequal(R, 2);
assert_checkerror('chol(A)', _('Positive definite matrix with a real diagonal expected.'), 'Nelson:chol:realDiagonalExpected');
%=============================================================================
[R, p] = chol(single([2 1; 1 3+1i]), 'lower');
asserts.isequal(p, 2);
asserts.isequal(R, single(sqrt(2)));
%=============================================================================
% a negative zero imaginary part is real
R = chol([complex(2, -0) 1; 1 2]);
asserts.isequal(R, [sqrt(2) 1/sqrt(2); 0 sqrt(1.5)]);
%=============================================================================
% real failure keeps the existing message
assert_checkerror('chol([1 2; 2 1])', _('Positive finite matrix expected.'), 'Nelson:chol:positiveFiniteMatrixExpected');
%=============================================================================
% sparse: the factor keeps all columns, rows stop before the failing pivot
[R, p] = chol(sparse([4 2 0; 2 5 1; 0 1 6+2i]));
asserts.isequal(p, 3);
asserts.istrue(issparse(R));
asserts.isequal(full(R), [2 1 0; 0 2 0.5]);
[R, p] = chol(sparse([4 2 0; 2 5 1; 0 1 6+2i]), 'lower');
asserts.isequal(p, 3);
asserts.isequal(full(R), [2 0; 1 2; 0 0.5]);
%=============================================================================
[R, p] = chol(sparse([-1 0; 0 1+1i]));
asserts.isequal(p, 1);
asserts.isequal(size(R), [0 2]);
[R, p] = chol(sparse([-1 0; 0 1+1i]), 'lower');
asserts.isequal(size(R), [2 0]);
%=============================================================================
[R, p] = chol(sparse([4 1+1i; 1-1i 2] + [0 0; 0 1i]));
asserts.isequal(p, 2);
asserts.isequal(full(R), [2 0.5+0.5i]);
%=============================================================================
[R, p] = chol(sparse([4 1 2; 1 3 1; 2 1 -5]));
asserts.isequal(p, 3);
asserts.isequal(size(R), [2 3]);
asserts.istrue(max(max(abs(full(R) - [2 0.5 1; 0 sqrt(2.75) 0.5 / sqrt(2.75)]))) <= 4 * eps);
%=============================================================================
[R, p] = chol(sparse([2 5i; 7 3+1i]), 'lower');
asserts.isequal(p, 2);
asserts.istrue(max(abs(full(R) - [sqrt(2); 7 / sqrt(2)])) <= 4 * eps);
%=============================================================================
assert_checkerror('chol(sparse([2 1; 1 3+1i]))', _('Positive finite matrix expected.'), 'Nelson:chol:positiveFiniteMatrixExpected');
%=============================================================================
% a failing real tridiagonal matrix keeps a square factor
[R, p] = chol(sparse([2 1 0 0; 1 -2 1 0; 0 1 2 1; 0 0 1 2]));
asserts.isequal(p, 2);
asserts.isequal(size(R), [4 4]);
asserts.isequal(full(R(1, :)), [sqrt(2) 1 / sqrt(2) 0 0]);
asserts.isequal(nnz(R(2:end, :)), 0);
[R, p] = chol(sparse([2 1 0; 3 2 1; 0 1 -5]), 'lower');
asserts.isequal(p, 2);
asserts.isequal(size(R), [3 3]);
asserts.isequal(full(R(:, 1)), [sqrt(2); 3 / sqrt(2); 0]);
[R, p] = chol(sparse(single([1 2; 2 1])));
asserts.isequal(full(R), single([1 2; 0 0]));
%=============================================================================
% partial bands and complex tridiagonal matrices do not
[R, p] = chol(sparse([2 1 0; 1 2 0; 0 0 -5]));
asserts.isequal(size(R), [2 3]);
[R, p] = chol(sparse([2 1 0; 0 2 1; 0 0 -5]));
asserts.isequal(size(R), [2 3]);
[R, p] = chol(sparse([2 1+1i 0; 1-1i 2 1; 0 1 -5]));
asserts.isequal(size(R), [2 3]);
[R, p] = chol(sparse(-1));
asserts.isequal(size(R), [0 1]);
%=============================================================================
