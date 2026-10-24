%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Compare LU, QR and eigenvalue decompositions of one matrix.
A = [4, 2, 1; 2, 5, 2; 1, 2, 3];
[L, U, P] = lu(A);
[Q, R] = qr(A);
[V, D] = eig(A);
disp(['LU error: ', mat2str(norm(P * A - L * U))]);
disp(['QR error: ', mat2str(norm(A - Q * R))]);
disp('Eigenvalues:');
disp(diag(D));
