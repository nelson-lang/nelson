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
% <--ENGLISH IMPOSED-->
%=============================================================================
M = [1, 2, 3, 4; 5 6 7 8; 8 9 10 11];
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
asserts.isequal(R, REF);
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
asserts.isequal(R, REF);
%=============================================================================
num = [-1 3 -4];
den = [-1 5 -7];
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
asserts.isequal(R, REF);
%=============================================================================
