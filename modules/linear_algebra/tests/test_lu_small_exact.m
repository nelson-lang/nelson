%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Small matrices are factored by an unblocked LU without fused multiply-add:
% lu, det, inv, rcond and the solvers give the same bits as the reference.
%=============================================================================
A = [1+1e-13 2 3; 4 5 6; 7 8 9];
[L, U, p] = lu(A, 'vector');
asserts.isequal(p, [3 1 2]);
asserts.isequal(U, [7 8 9; 0 0.85714285714274285 1.7142857142855856; 0 0 -4.9960036108132044e-14]);
asserts.isequal(L, [1 0 0; 0.14285714285715714 1 0; 0.5714285714285714 0.50000000000006695 1]);
asserts.isequal(rcond(A), 6.9388939039067955e-16);
%=============================================================================
Z = [1+2i 2-1i 0.5; -1+1i 3 2i; 0.25-1i 1+1i -2];
[L, U, p] = lu(Z, 'vector');
asserts.isequal(p, [1 3 2]);
REF_U = [1+2i, 2-1i, 0.5; 0, 2+1.25i, -1.825+0.14999999999999999i; ...
  0, 0, 0.68089887640449442+0.14943820224719095i];
asserts.isequal(U, REF_U);
REF_L = [1 0 0; -0.34999999999999998-0.29999999999999999i 1 0; ...
  0.20000000000000001+0.59999999999999998i 0.4943820224719101-0.8089887640449438i 1];
asserts.isequal(L, REF_L);
asserts.isequal(det(Z), 1.1249999999999998 - 3.5i);
%=============================================================================
% rectangular
R = [0.3 -1.2 2.5 0.7 -0.4; 1.1 0.9 -0.6 2.2 1.3; -2.4 0.5 1.7 -0.8 0.6];
[L, U, p] = lu(R, 'vector');
asserts.isequal(p, [3 1 2]);
REF_U = [-2.3999999999999999 0.5 1.7 -0.80000000000000004 0.59999999999999998; ...
  0 -1.1375 2.7124999999999999 0.59999999999999998 -0.32500000000000001; ...
  0 0 2.8717948717948718 2.4289377289377292 1.2523809523809526];
asserts.isequal(U, REF_U);
asserts.isequal(L, [1 0 0; -0.125 1 0; -0.45833333333333337 -0.9926739926739927 1]);
%=============================================================================
% single
S = single([0.3 -1.2 2.5; 1.1 0.9 -0.6; -2.4 0.5 1.7]);
[L, U] = lu(S);
asserts.isequal(class(U), 'single');
asserts.isequal(U, single([-2.4000001 0.5 1.70000005; 0 -1.13750005 2.7125001; 0 0 2.87179446]));
asserts.isequal(det(S), single(7.83999968));
%=============================================================================
% det is the diagonal product of the same factor
asserts.isequal(det(magic(4)), -1.4495071809506048e-12);
asserts.isequal(det([1 2 3; 4 5 6; 7 8 9]), 6.6613381477509392e-16);
%=============================================================================
% inv
REF = [1.4444444444444442 -1.2222222222222219 -0.55555555555555558; ...
  -0.77777777777777768 0.88888888888888884 0.22222222222222221; ...
  0.33333333333333337 -0.66666666666666663 0.33333333333333331];
asserts.isequal(inv([4 7 2; 3 6 1; 2 5 3]), REF);
%=============================================================================
% left and right division use the same factor and substitution order
warning('off', 'Nelson:nearlySingularMatrix');
asserts.isequal(A \ [1; 2; 3], [0.001111111111111264; -0.0022222222222223293; 0.33444444444444443]);
asserts.isequal([1 2 3] / A, [-4.4408920985006262e-16 2 -0.99999999999999989]);
asserts.isequal(magic(4) / magic(4), eye(4));
warning('on', 'Nelson:nearlySingularMatrix');
%=============================================================================
% a zero entry of U skips its update, so NaN does not spread there
asserts.isequal([1 0; NaN 1] \ [1; 1], [1; NaN]);
%=============================================================================
% a singular single matrix gets an exact zero pivot
warning('off', 'Nelson:singularMatrix');
lastwarn('');
X = single([1 2 3; 2 2 2]) / single([1 2 3; 4 5 6; 7 8 9]);
[~, id] = lastwarn();
asserts.isequal(id, 'Nelson:singularMatrix');
asserts.istrue(all(isnan(X(:))));
warning('on', 'Nelson:singularMatrix');
%=============================================================================
% complex triangular and diagonal divisors use the same complex division as ./
dc = [2+1i, 3-1i, 0.5i];
Bc = [1, 1i, 2-1i; 1-1i, 3, 1i];
asserts.isequal(Bc / diag(dc), Bc ./ dc);
asserts.isequal(diag(dc) \ Bc.', Bc.' ./ dc.');
T = [2+1i 0 0; 1i 3-1i 0; 1 2 0.5i];
REF = [0.40000000000000002-0.20000000000000001i, -0.12+0.16i, -2.2400000000000002-3.6800000000000002i];
asserts.isequal(Bc(1, :) / T.', REF);
asserts.isequal(T \ Bc(1, :).', REF.');
%=============================================================================
% a singular system keeps 0/0 in the substitution
warning('off', 'Nelson:singularMatrix');
asserts.istrue(isequaln([1 2; 2 4] \ [1; 2], [NaN; NaN]));
asserts.istrue(isequaln([1 0; Inf 1] \ [0; 1], [0; NaN]));
asserts.istrue(isequaln([0 1] / [1 0; Inf 1], [-Inf 1]));
warning('on', 'Nelson:singularMatrix');
%=============================================================================
% larger matrices keep LAPACK: results still solve the system
rng(1);
B = rand(40) + 40 * eye(40);
[L, U, p] = lu(B, 'vector');
asserts.isapprox(L * U, B(p, :), 1e-12);
asserts.isapprox(det(B), prod(diag(U)) * det(eye(40)(p, :)), 1e-10);
%=============================================================================
