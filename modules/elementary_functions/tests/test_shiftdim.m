%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
rng('default');
A = rand(4, 2, 3, 5);
B = shiftdim(A, 2);
R = size(B);
REF = [3 5 4 2];
asserts.isequal(R, REF);
%=============================================================================
C = shiftdim(A, -2);
R = size(C);
REF = [1 1 4 2 3 5];
asserts.isequal(R, REF);
%=============================================================================
A = rand(1, 1, 3, 2, 4);
[B, nshifts] = shiftdim(A);
asserts.isequal(nshifts, 2);
asserts.isequal(size(B), [3 2 4]);
%=============================================================================
C = shiftdim(B, 2);
asserts.isequal(size(C), [4 3 2]);
%=============================================================================
D = shiftdim(C, -1);
asserts.isequal(size(D), [1 4 3 2]);
%=============================================================================
rng('default');
X = rand(1, 1, 4, 2);
[Y, N] = shiftdim(X);
Y_REF = [0.8147 0.6324;
    0.9058 0.0975;
    0.1270 0.2785;
    0.9134 0.5469];
N_REF = 2;
asserts.isapprox(Y, Y_REF, 1e-4);
asserts.isequal(N, N_REF);
%=============================================================================
[shifted, shiftCount] = shiftdim(reshape(1:3, [1 1 3]));
asserts.isequal(size(shifted), [3 1]);
asserts.isequal(shiftCount, 2);
%=============================================================================
% Negative shift drops trailing singleton dimensions (matches ndims).
z = shiftdim([1; 2; 3], -1);
asserts.isequal(size(z), [1 3]);
asserts.isequal(ndims(z), 2);
z2 = shiftdim([1 2 3], -1);
asserts.isequal(size(z2), [1 1 3]);
asserts.isequal(ndims(z2), 3);
asserts.isequal(ndims(shiftdim(5, -1)), 2);
%=============================================================================
