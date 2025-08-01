%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
numerator = [2, 0];
denominator = [4, 0, 3, -1];
sys = tf(numerator, denominator);
R = evalc('display(sys)');
REF =  ' 
sys =
 
        2 s
  ———————————————
  4 s^3 + 3 s - 1
 
Continuous-time transfer function.
 
';
assert_isequal(R, REF);
%=============================================================================
numerator = [2, 0];
denominator = [4, 0, 3, -1];
ts = 1.5;
sys = tf(numerator, denominator, ts);
R = evalc('display(sys)');
REF =  ' 
sys =
 
        2 z
  ———————————————
  4 z^3 + 3 z - 1
 
Sample time: 1.5000 seconds
Discrete-time transfer function.
 
';
assert_isequal(R, REF);
%=============================================================================
numerator = {[2, 0], [1, 3]};
denominator = {[4, 0, 3, -1], [1 , 3, 5]};
sys = tf(numerator, denominator);
R = evalc('display(sys)');
REF =  ' 
sys =
 
  From input 1 to output:
        2 s
  ———————————————
  4 s^3 + 3 s - 1
 
  From input 2 to output:
      s + 3
  —————————————
  s^2 + 3 s + 5
 
Continuous-time transfer function.
 
';
assert_isequal(R, REF);
Numerator_REF = {[0  0  2  0], [0  1  3]};
Denominator_REF = {[4  0  3  -1], [1  3  5]};
assert_isequal(sys.Numerator, Numerator_REF);
assert_isequal(sys.Denominator, Denominator_REF);
%=============================================================================
numerator = {[2, 0], [1, 3], [4, 6]};
denominator = {[4, 0, 3, -1], [1 , 3, 5]};
assert_checkerror('sys = tf(numerator, denominator);', _('Numerator and Denominator must have compatible sizes.'));
%=============================================================================
M = [1,2,3,4; 5 6 7 8; 8 9 10 11];
sys = tf(M);
R = evalc('display(sys)');
REF = ' 
sys =
 
  From input 1 to output:
  1:  1
 
  2:  5
 
  3:  8
 
  From input 2 to output:
  1:  2
 
  2:  6
 
  3:  9
 
  From input 3 to output:
  1:  3
 
  2:  7
 
  3:  10
 
  From input 4 to output:
  1:  4
 
  2:  8
 
  3:  11
 
     Static gain.
 
';
assert_isequal(R, REF);
%=============================================================================
num = [3 4];
den = [3 1 5];
Ts = 0.2;
sys = tf(num, den, Ts);
sys.Variable = 'z^-1';
R = evalc('display(sys)');
REF =   ' 
sys =
 
   3 z^-1 + 4 z^-2
  —————————————————
  3 + z^-1 + 5 z^-2
 
Sample time: 0.2000 seconds
Discrete-time transfer function.
 
'; 
assert_isequal(R, REF);
%=============================================================================
num = [-1 3 -4];
den = [ -1 5 -7];
Ts = 0.2;
sys = tf(num, den, Ts);
sys.Variable = 'z^-1';
R = evalc('display(sys)');
REF = ' 
sys =
 
  - 1 + 3 z^-1 - 4 z^-2
  —————————————————————
  - 1 + 5 z^-1 - 7 z^-2
 
Sample time: 0.2000 seconds
Discrete-time transfer function.
 
' ;
assert_isequal(R, REF);
%=============================================================================
sys1 = tf([1 -1],[1 1]);
sys2 = tf([1 2],[1 4 5]);
sys = [sys1; sys2];
R = evalc('display(sys)');
REF =  ' 
sys =
 
  1:
   s - 1
  ———————
   s + 1
 
  2:
      s + 2
  —————————————
  s^2 + 4 s + 5
 
Continuous-time transfer function.
 
';
assert_isequal(R, REF)
%=============================================================================
sys1 = tf([1 -1],[1 1]);
sys2 = tf([1 2],[1 4 5]);
sys = [sys1, sys2];
R = evalc('display(sys)');
REF =  ' 
sys =
 
  From input 1 to output:
   s - 1
  ———————
   s + 1
 
  From input 2 to output:
      s + 2
  —————————————
  s^2 + 4 s + 5
 
Continuous-time transfer function.
 
';
assert_isequal(R, REF)
%=============================================================================
numerator = [1,0,0];
denominator = [1,2,3];
ts = 0.1;
sys1 = tf(numerator,denominator,ts);
R = evalc('display(sys1)');
REF = ' 
sys1 =
 
       z^2
  —————————————
  z^2 + 2 z + 3
 
Sample time: 0.1000 seconds
Discrete-time transfer function.
 
';
assert_isequal(R, REF)
%=============================================================================
sys2 = sys1;
sys2.Variable = 'z^-1';
R = evalc('display(sys2)');
REF =  ' 
sys2 =
 
           1
  ———————————————————
  1 + 2 z^-1 + 3 z^-2
 
Sample time: 0.1000 seconds
Discrete-time transfer function.
 
';
assert_isequal(R, REF)
%=============================================================================
Fs = 5;
dt = 1/Fs;
A = [cos(dt) sin(dt);-sin(dt) cos(dt)];
B = [1-cos(dt);sin(dt)];
C = [-1 0];
D = 1;
sysIn = ss(A, B, C, D);

sys = tf(sysIn);
[N, D] = tfdata(sys);

N_REF = [1.0000   -1.9801    0.9801];
D_REF = [1.0000   -1.9601    1.0000];

assert_isapprox(N, N_REF, 1e-4);
assert_isapprox(D, D_REF, 1e-4);
%=============================================================================
A = [-1     0     0;   0    -4  -2.5;   0     2     0];
B = [2   0; 0   2; 0   0];
C = [-1  0.5  0.5];
D = [ 1   0];
sysIn = ss(A, B, C, D);

sys = tf(sysIn);
[N, D] = tfdata(sys);

N_REF = {[1  -1], [1.0000  5.0000  7.0000]};
D_REF = {[1  1], [1.0000  4.0000  5.0000]};

assert_isapprox(N{1}, N_REF{1}, 1e-4);
assert_isapprox(D{1}, D_REF{1}, 1e-4);
assert_isapprox(N{2}, N_REF{2}, 1e-4);
assert_isapprox(D{2}, D_REF{2}, 1e-4);

%=============================================================================
