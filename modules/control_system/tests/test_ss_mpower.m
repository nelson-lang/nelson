%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [0 1; 0 0];
B = [0; 1];
C = [1 0];
D = 0;
sys = ss(A, B, C, D);
sys2 = sys ^ 3;
REF_A = [0   1   0   0   0   0;
0   0   1   0   0   0;
0   0   0   1   0   0;
0   0   0   0   1   0;
0   0   0   0   0   1;
0   0   0   0   0   0];
REF_B = [ 0; 0; 0; 0; 0; 1];
REF_C = [1   0   0   0   0   0];
REF_D = 0;
assert_isequal(sys2.A, REF_A)
assert_isequal(sys2.B, REF_B)
assert_isequal(sys2.C, REF_C)
assert_isequal(sys2.D, REF_D)
%=============================================================================
A = [0 1; 0 0];
B = [0; 1];
C = [1 0];
D = 0;
sys = ss(A, B, C, D);
sys2 = sys ^ -3;

REF_A = [0   1   0   0   0   0   0;
0   0   1   0   0   0   0;
0   0   0   1   0   0   0;
0   0   0   0   1   0   0;
0   0   0   0   0   1   0;
0   0   0   0   0   0   1;
1   0   0   0   0   0   0];
REF_B = [ 0;  0;  0;  0;  0;  0; -1];
REF_C = [0   0   0   0   0   0   1]
REF_D = 0;
REF_E = [1   0   0   0   0   0   0;
0   1   0   0   0   0   0;
0   0   1   0   0   0   0;
0   0   0   1   0   0   0;
0   0   0   0   1   0   0;
0   0   0   0   0   1   0;
0   0   0   0   0   0   0];
assert_isequal(sys2.A, REF_A)
assert_isequal(sys2.B, REF_B)
assert_isequal(sys2.C, REF_C)
assert_isequal(sys2.D, REF_D)
assert_isequal(sys2.E, REF_E)
%=============================================================================
