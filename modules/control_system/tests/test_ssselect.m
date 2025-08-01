%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [33,2,5; 23,200,2; 9,2,45];
B = [4,5; 12,5; 82,1];
C = [34,56,2; 6,2,112];
D = [2,0; 0,19];
sys1 = ss(A, B, C, D);
inputs = 1;
outputs = 1;

R = ssselect(sys1, inputs, outputs);
A_REF = A;
B_REF = [4;12;82];
C_REF = [34,56,2];
D_REF = 2;
REF = ss(A_REF, B_REF, C_REF, D_REF);

assert_isequal(R, REF);
%=============================================================================
