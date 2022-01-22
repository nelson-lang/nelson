%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
format('shortE')
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

   1.0000e+09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = pi');
REF =  '
A =

   3.1416e+00

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

   1.0000e+09   1.0000e+99

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = [6.5574e-04, 6.5574e-04]');
REF = '
A =

   6.5574e-04   6.5574e-04

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = [0.3729   6.5574e-04    0.1393]');
REF = '
A =

   3.7290e-01   6.5574e-04   1.3930e-01

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

   2.2200e-01   1.0000e+00   2.0000e+00
            0   2.2200e-01   4.0000e+00
   5.0000e+00   6.0000e+00   2.2200e-01

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
