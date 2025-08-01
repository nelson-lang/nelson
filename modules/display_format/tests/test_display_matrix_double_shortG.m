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
format('shortG')
%=============================================================================
R = evalc('A = 1');
REF =  '
A =

     1

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 10');
REF =  '
A =

    10

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e2');
REF =  '
A =

   100

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e3');
REF =  '
A =

        1000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e4');
REF =  '
A =

       10000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e5');
REF =  '
A =

      100000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e6');
REF =  '
A =

     1000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e7');
REF =  '
A =

    10000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e8');
REF =  '
A =

   100000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e9');
REF =  '
A =

        1e+09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = pi');
REF =  '
A =

       3.1416

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = NaN');
REF =  '
A =

   NaN

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = Inf');
REF =  '
A =

   Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -Inf');
REF =  '
A =

  -Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = [0 eps]');
REF =  '
A =

            0   2.2204e-16

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = [1e9 1e99]');
REF =  '
A =

        1e+09        1e+99

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = [6.5574e-04, 6.5574e-04]');
REF = '
A =

   0.00065574   0.00065574

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = [0.3729   6.5574e-04    0.1393]');
REF = '
A =

       0.3729   0.00065574       0.1393

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = eye(3,3)');
REF = '
A =

     1     0     0
     0     1     0
     0     0     1

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = eye(3,3) * eps');
REF = '
A =

   2.2204e-16            0            0
            0   2.2204e-16            0
            0            0   2.2204e-16

';
assert_isequal(R, REF)
%=============================================================================
A = [0.2220, 1, 2;
     0,  0.2220, 4;
     5, 6, 0.2220];
R = evalc('A');
REF = '
A =

        0.222            1            2
            0        0.222            4
            5            6        0.222

';
assert_isequal(R, REF)
%=============================================================================
A = ones(3,20);
R = evalc('A');
REF = '
A =

  Columns 1 through 13

     1     1     1     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1     1     1     1
     1     1     1     1     1     1     1     1     1     1     1     1     1

  Columns 14 through 20

     1     1     1     1     1     1     1
     1     1     1     1     1     1     1
     1     1     1     1     1     1     1

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = [Inf Inf]');
REF =  '
A =

   Inf   Inf

';
assert_isequal(R, REF)
%=============================================================================
