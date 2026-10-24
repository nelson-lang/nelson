%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('hess'), -1);
asserts.isequal(nargout('hess'), -1);
%=============================================================================
A = magic(4);
[P, H] = hess(A);
% A = P * H * P'
asserts.istrue(norm(P * H * P' - A) < 1e-10);
% P orthogonal
asserts.istrue(norm(P' * P - eye(4)) < 1e-10);
% H is upper Hessenberg : H(i,j) == 0 for i > j+1
asserts.istrue(norm(tril(H, -2)) < 1e-10);
% Hessenberg reduction preserves the spectrum
asserts.istrue(norm(sort(eig(H)) - sort(eig(A))) < 1e-8);
%=============================================================================
% Single output returns H only.
H2 = hess(A);
asserts.istrue(norm(H2 - H) < 1e-12);
%=============================================================================
% Larger real matrix.
B = magic(5);
[P, H] = hess(B);
asserts.istrue(norm(P * H * P' - B) < 1e-10);
asserts.istrue(norm(tril(H, -2)) < 1e-10);
asserts.istrue(norm(P' * P - eye(5)) < 1e-10);
%=============================================================================
% Complex matrix (P unitary).
A = [4 1 -2 2; 1 2 0 1; -2 0 3 -2; 2 1 -2 -1] + ...
  1i * [0 1 0 0; -1 0 2 0; 0 -2 0 1; 0 0 -1 0];
[P, H] = hess(A);
asserts.istrue(norm(P * H * P' - A) < 1e-10);
asserts.istrue(norm(P' * P - eye(4)) < 1e-10);
asserts.istrue(norm(tril(H, -2)) < 1e-10);
%=============================================================================
% Non-square input raises an error.
asserts.checkerror('hess([1 2 3])', sprintf(_('%s expected.'), 'Square matrix'));
%=============================================================================
