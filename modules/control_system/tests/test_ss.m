%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('ss'), -1);
assert_isequal(nargout('ss'), -1);
%=============================================================================
A = [-15, -20; 10, 0];
B = [5; 0];
C = [0, 10];
D = 0;
sys = ss(A, B, C, D);
assert_isequal(class(sys), 'ss');
assert_isequal(sys.A, A);
assert_isequal(sys.B, B);
assert_isequal(sys.C, C);
assert_isequal(sys.D, D);
assert_isequal(sys.Ts, 0);
assert_isequal(sys.UserData, []);
%=============================================================================
A = [1 2 3; 4 5 6; 7 8 9];
B = [10; 11; 12];
C = [];
D = [];
sys = ss (A, B, C, D);
assert_isequal(sys.A, A);
assert_isequal(sys.B, B);
assert_isequal(sys.C, zeros(0, 3));
assert_isequal(sys.D, zeros(0, 1));
%=============================================================================
assert_checkerror('sys = ss([], 3, [], [])', _('The number of rows in matrices A and B must be equal.'));
%=============================================================================
A = [0  10; -20 -30];
B =[0 10];
C =[10 0];
D = 0;
assert_checkerror('sys = ss(A, B, C, D)', _('The number of rows in matrices A and B must be equal.'));
%=============================================================================
A = [ 1  0.0000  4.9200  -4.9200  0.0000  0.0000  0.0000;
2  -12.500  0.0000   0.0000  0.0000  0.0000  0.0000;
0.0000   3.3300 -3.3300   0.0000  0.0000  0.0000  0.0000;
3   0.0000  0.0000   0.0000 -0.5450  0.0000  0.0000;
5.0000   0.0000  0.0000   4.9200 -0.04165 0.0000  4.9200;
0.0000   6.0000  0.0000   0.0000 -5.2100 -12.500  0.0000;
0.0000   0.0000  6.0000   0.0000  0.0000  3.3300 -3.3300];
B = [  0.0000   0.0000;
23.5000   0.0000;
0.0000   0.0000;
0.0000   0.0000;
0.0000   0.0000;
0.0000   23.500;
0.0000   0.0000];
C = [  1.0000   0.0000  0.0000   0.0000  0.0000  0.0000  0.0000;
0.0000   0.0000  0.0000   1.0000  0.0000  0.0000  0.0000;
0.0000   0.0000  0.0000   0.0000  1.0000  0.0000  0.0000];
D = [];
sys = ss(A, B, C, D);
assert_isapprox(sys.A, A, 1e-3);
assert_isapprox(sys.B, B, 1e-3);
assert_isapprox(sys.C, C, 1e-3);
assert_isequal(sys.D, zeros(3, 2));
%=============================================================================
D = [20, 40; 30, 50];
sys = ss(D);
assert_isequal(sys.A, []);
assert_isequal(sys.B, zeros(0, 2));
assert_isequal(sys.C, zeros(2, 0));
assert_isequal(sys.D, D);
assert_isequal(sys.Ts, 0);
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sysIn = tf(num, den, Ts)
sys = ss(sysIn);

A_REF = [-0.3333   -1.6667;  1.0000         0];
B_REF = [2;   0];
C_REF = [0.5000    0.6667];
D_REF = 0;
Ts_REF = Ts; 

assert_isapprox(sys.A, A_REF, 1e-4);
assert_isapprox(sys.B, B_REF, 1e-4);
assert_isapprox(sys.C, C_REF, 1e-4);
assert_isapprox(sys.D, D_REF, 1e-4);
assert_isapprox(Ts, Ts_REF, 1e-4);
%=============================================================================
