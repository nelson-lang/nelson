%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1 2; 3 4];
B = [1 0; 0 1];
C = [1 1; 1 1];
D = [1 2; 3 4];
sys1 = ss(A, B, C, D);
sys2 = ss(A, B, C, D);
R = sys1 \ sys2;

REF_A = [1   2   1   0   0   0;
3   4   0   1   0   0;
1   1   1   2  -1  -1;
1   1   3   4  -1  -1;
0   0   0   0   1   2;
0   0   0   0   3   4];
REF_B = [0   0;
0   0;
-1  -2;
-3  -4;
1   0;
0   1];
REF_C = [0   0   1   0   0   0;
0   0   0   1   0   0];
REF_D = [0   0;
0   0];
REF_E = [1   0   0   0   0   0;
0   1   0   0   0   0;
0   0   0   0   0   0;
0   0   0   0   0   0;
0   0   0   0   1   0;
0   0   0   0   0   1];
REF = ss(REF_A, REF_B, REF_C, REF_D);
REF.E = REF_E;
assert_isequal(R, REF);
