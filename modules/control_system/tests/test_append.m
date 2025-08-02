%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys1 = tf(2, [1 3 0]);
sys2 = tf(1, [8 2 1]);
sys = append(sys1, 10, sys2);

num_REF = {[0 0 2], [ 0], [    0]; [    0], [10], [    0]; [    0], [ 0], [0 0 1]};
assert_isequal(sys.Numerator, num_REF);

den_REF = {[1 3 0], [1], [    1]; [    1], [1], [    1]; [    1], [1], [8 2 1]};
assert_isequal(sys.Denominator, den_REF);
%=============================================================================
sys1 = ss(0, 1, 1, 0);
sys2 = ss(5, 4, 3, 2);
sys = append(sys1, 10, sys2);

A_REF = [0     0;0     5];
B_REF = [1     0     0;0     0     4];
C_REF = [1     0;0     0;0     3];
D_REF = [0     0     0;0    10     0;0     0     2];
assert_isequal(sys.A, A_REF);
assert_isequal(sys.B, B_REF);
assert_isequal(sys.C, C_REF);
assert_isequal(sys.D, D_REF);
%=============================================================================
sys1 = tf(1,[1 0]);
sys2 = tf([1 -1], [4 2]);
sys = append(sys1, 10, sys2)

num_REF = {[0 1], [ 0], [   0]; [  0], [10], [   0];[  0], [ 0], [1 -1]};
den_REF = {[1 0], [1], [  1]; [  1], [1], [  1]; [  1], [1], [4 2]};
assert_isequal(sys.Numerator, num_REF);
assert_isequal(sys.Denominator, den_REF);
%=============================================================================
