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
format('short')
%=============================================================================
R = evalc('A = 1');
REF =   '
A =

     1

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -1');
REF =   '
A =

    -1

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 10');
REF =   '
A =

    10

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -10');
REF =   '
A =

   -10

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 100');
REF = '
A =

   100

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -100');
REF = '
A =

  -100

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1000');
REF =   '
A =

        1000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -1000');
REF = '
A =

       -1000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 10000');
REF =    '
A =

       10000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -10000');
REF =     '
A =

      -10000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 100000');
REF = '
A =

      100000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -100000');
REF = '
A =

     -100000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1000000');
REF = '
A =

     1000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -1000000');
REF = '
A =

    -1000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 100000000');
REF = '
A =

   100000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -100000000');
REF = '
A =

  -100000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1000000000');
REF = '
A =

   1.0000e+09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -1000000000');
REF = '
A =

  -1.0000e+09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e9');
REF = '
A =

   1.0000e+09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -1e9');
REF = '
A =

  -1.0000e+09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e99');
REF = '
A =

   1.0000e+99

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -1e99');
REF = '
A =

  -1.0000e+99

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 99e99');
REF = '
A =

  9.9000e+100

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = 1e999');
REF =     '
A =

   Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -1e999');
REF = '
A =

  -Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = pi');
REF = '
A =

    3.1416

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -pi');
REF = '
A =

   -3.1416

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = pi*100');
REF = '
A =

  314.1593

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = pi*1000');
REF = '
A =

   3.1416e+03

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -pi*1000');
REF = '
A =

  -3.1416e+03

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -pi*1e999');
REF = '
A =

  -Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = eps');
REF = '
A =

   2.2204e-16

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = -eps');
REF = '
A =

  -2.2204e-16

';
assert_isequal(R, REF)
%=============================================================================
