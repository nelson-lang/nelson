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
rng('default')
format('long')
%=============================================================================
A = single(rand(3,3));
A(1) = 0;
R = evalc('A');
REF =  '
A =

  3×3 single matrix

           0   0.9133759   0.2784982
   0.9057919   0.6323593   0.5468815
   0.1269868   0.0975404   0.9575068

';
assert_isequal(R, REF)
%=============================================================================
rng('default')
A = single(complex(rand(3,3), rand(3,3)));
A(1) = single(complex(0, 99));
R = evalc('A');
REF = '
A =

  3×3 single matrix

  0.0000000 +99.0000000i  0.9133759 + 0.9571670i  0.2784982 + 0.1418863i
  0.9057919 + 0.1576131i  0.6323593 + 0.4853756i  0.5468815 + 0.4217613i
  0.1269868 + 0.9705928i  0.0975404 + 0.8002805i  0.9575068 + 0.9157355i

';
assert_isequal(R, REF)
%=============================================================================
rng('default')
A = single(complex(rand(3,3), rand(3,3)));
A(1) = single(complex(0, 199));
R = evalc('A');
REF = '
A =

  3×3 single matrix

   1.0e+02 *

  0.0000000 + 1.9900000i  0.0091338 + 0.0095717i  0.0027850 + 0.0014189i
  0.0090579 + 0.0015761i  0.0063236 + 0.0048538i  0.0054688 + 0.0042176i
  0.0012699 + 0.0097059i  0.0009754 + 0.0080028i  0.0095751 + 0.0091574i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(ones(0,4))');
REF = '
A =

  0×4 empty single matrix

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(eye(3,4))');
REF = '
A =

  3×4 single matrix

     1     0     0     0
     0     1     0     0
     0     0     1     0

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('single([8140.1 905.2])');
REF =  '
ans =

  1×2 single row vector

   1.0e+03 *

   8.1401001   0.9052000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('single([81.1 90.2])');
REF =  '
ans =

  1×2 single row vector

  81.0999985  90.1999969

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(eps)');
REF =  '
A =

  single

   2.2204460e-16

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(NaN)');
REF = '
A =

  single

   NaN

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(Inf)');
REF =  '
A =

  single

   Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(-Inf)');
REF = '
A =

  single

  -Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single([1e-3 1.1e-3 1.2e-3])');
REF = '
A =

  1×3 single row vector

   0.0010000   0.0011000   0.0012000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single([1e-4 1.1e-4 1.2e-4])');
REF = '
A =

  1×3 single row vector

   1.0e-03 *

   0.1000000   0.1100000   0.1200000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single([1e4 1.1e4 1.2e4])');
REF = '
A =

  1×3 single row vector

       10000       11000       12000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single([10 10 13])');
REF =  '
A =

  1×3 single row vector

    10    10    13

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e99)');
REF = '
A =

  single

   Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e10)');
REF = '
A =

  single

   1.0000000e+10

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e9)');
REF = '
A =

  single

   1.0000000e+09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e8)');
REF = '
A =

  single

   100000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e7)');
REF = '
A =

  single

    10000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e6)');
REF = '
A =

  single

     1000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e5)');
REF = '
A =

  single

      100000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e4)');
REF = '
A =

  single

       10000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e3)');
REF = '
A =

  single

        1000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e2)');
REF = '
A =

  single

   100

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1.002e2)');
REF = '
A =

  single

   1.0020000e+02

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e1)');
REF = '
A =

  single

    10

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1)');
REF = '
A =

  single

     1

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-1)');
REF = '
A =

  single

   0.1000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-2)');
REF =  '
A =

  single

   0.0100000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-3)');
REF =  '
A =

  single

   0.0010000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-4)');
REF =  '
A =

  single

   9.9999997e-05

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-5)');
REF = '
A =

  single

   9.9999997e-06

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-6)');
REF = '
A =

  single

   1.0000000e-06

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-7)');
REF = '
A =

  single

   1.0000000e-07

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-8)');
REF =  '
A =

  single

   9.9999999e-09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-9)');
REF =  '
A =

  single

   9.9999997e-10

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-120)');
REF =  '
A =

  single

     0

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(1e-999)');
REF =  '
A =

  single

     0

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = single(pi*1e-4)');
REF = '
R =

  single

   3.1415926e-04

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = single(pi*1e-3)');
REF = '
R =

  single

   0.0031416

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = single(-pi*1e-4)');
REF =  '
R =

  single

  -3.1415926e-04

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = single(-pi*1e-3)');
REF =  '
R =

  single

  -0.0031416

';
assert_isequal(R, REF)
%=============================================================================
A = zeros(4, 4);
A(3, 3) = 1e-3;
R = evalc('R = single(A)');
REF =  '
R =

  4×4 single matrix

           0           0           0           0
           0           0           0           0
           0           0   0.0010000           0
           0           0           0           0

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = single([-1 -20 -4])');
REF =  '
R =

  1×3 single row vector

    -1   -20    -4

'; 
assert_isequal(R, REF)
%=============================================================================
