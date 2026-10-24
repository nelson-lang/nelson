%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
wn = 5;
z = 0.7;
%=============================================================================
[A, B, C, D] = ord2(wn, z);
A_REF = [0 1; -25 -7];
B_REF = [0; 1];
C_REF = [1 0];
D_REF = 0;
asserts.isequal(A, A_REF);
asserts.isequal(B, B_REF);
asserts.isequal(C, C_REF);
asserts.isequal(D, D_REF);
%=============================================================================
[num, den] = ord2(wn, z);
num_REF = 1;
den_REF = [1 7 25];
asserts.isequal(num, num_REF)
asserts.isequal(den, den_REF)
%=============================================================================
