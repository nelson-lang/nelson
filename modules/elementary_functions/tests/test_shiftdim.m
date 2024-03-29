%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rng('default');
A = rand(4,2,3,5);
B = shiftdim(A,2);
R = size(B);
REF = [3     5     4     2];
assert_isequal(R, REF);
%=============================================================================
C = shiftdim(A, -2); 
R = size(C);
REF = [     1     1     4     2     3     5];
assert_isequal(R, REF);
%=============================================================================
A = rand(1,1,3,2,4);
[B, nshifts] = shiftdim(A);
assert_isequal(nshifts, 2);
assert_isequal(size(B), [     3     2     4]);
%=============================================================================
C = shiftdim(B,2);
assert_isequal(size(C), [     4     3     2]);
%=============================================================================
D = shiftdim(C, -1); 
assert_isequal(size(D), [     1     4     3     2]);
%=============================================================================
rng('default');
X = rand (1, 1, 4, 2);
[Y, N] = shiftdim(X);
Y_REF = [0.8147    0.6324;
    0.9058    0.0975;
    0.1270    0.2785;
    0.9134    0.5469];
N_REF = 2;
assert_isapprox(Y, Y_REF, 1e-4);
assert_isequal(N, N_REF);
%=============================================================================