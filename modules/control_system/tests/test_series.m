%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys1 = tf(2, [1 3 0]);
sys2 = tf(1, [8 2 1]);
sys = series(sys1, sys2);
REF = tf(2, [8 26 7 3 0]);
assert_isequal(sys, REF);
%=============================================================================
outputs1 = 1;
inputs2 = 1;
sys = series(sys1, sys2, outputs1, inputs2);
REF = tf(2, [8 26 7 3 0]);
assert_isequal(sys, REF);
%=============================================================================
[A, B, C, D] = ord2(1, 3);
sys1 = ss(A, B, C, D);
[A, B, C, D] = ord2(3, 6);
sys2 = ss(A, B, C, D)
outputs1 = 1;
inputs2 = 1;
sys = series(sys1, sys2, outputs1, inputs2);

A_REF = [0     1     0     0;-9   -36     1     0;0     0     0     1;0     0    -1    -6];
B_REF = [0; 0; 0; 1];
C_REF = [1     0     0     0];
D_REF = 0;
sys_REF = ss(A_REF, B_REF, C_REF, D_REF);

assert_isequal(sys, sys_REF);
%=============================================================================
