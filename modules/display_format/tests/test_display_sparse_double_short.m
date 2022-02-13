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
R = evalc('A = sparse([6.5574e-04])');
REF = '
A =

    (1,1)      6.5574e-04

';
assert_isequal(R, REF)
%=============================================================================
A = sparse([]);
A(1, 1) = 1;
A(1000, 20) = 3;
A(20,1000) = 4;
R = evalc('A');
REF = '
A =

       (1,1)           1
     (1000,20)         3
     (20,1000)         4

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([6.5574e-04, 6.5574e-04])');
REF = '
A =

    1.0e-03 *

    (1,1)       0.6557
    (1,2)       0.6557

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([0.3729   6.5574e-04    0.1393])');
REF =  '
A =

    (1,1)       0.3729
    (1,2)       0.0007
    (1,3)       0.1393

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([])');
REF = '
A =

  0×0 empty sparse double matrix

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(ones(0,4))');
REF = '
A =

  0×4 empty sparse double matrix

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(10000,5000)');
REF = '
A =

    All zero sparse: 10000×5000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(eps)');
REF = '
A =

    (1,1)      2.2204e-16

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(NaN)');
REF = '
A =

    (1,1)      NaN

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(Inf)');
REF = '
A =

    (1,1)      Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(-Inf)');
REF = '
A =

    (1,1)     -Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-3 1.1e-3 1.2e-3])');
REF = '
A =

    (1,1)       0.0010
    (1,2)       0.0011
    (1,3)       0.0012

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-4 1.1e-4 1.2e-4])');
REF = '
A =

    1.0e-03 *

    (1,1)       0.1000
    (1,2)       0.1100
    (1,3)       0.1200

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e4 1.1e4 1.2e4])');
REF =  '
A =

    (1,1)          10000
    (1,2)          11000
    (1,3)          12000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([10 10 13])');
REF = '
A =

    (1,1)       10
    (1,2)       10
    (1,3)       13

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e99)');
REF = '
A =

    (1,1)      1.0000e+99

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e10)');
REF = '
A =

    (1,1)      1.0000e+10

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e9)');
REF = '
A =

    (1,1)      1.0000e+09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e8)');
REF =  '
A =

    (1,1)      100000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e7)');
REF = '
A =

    (1,1)       10000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e6)');
REF = '
A =

    (1,1)        1000000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e5)');
REF = '
A =

    (1,1)         100000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e4)');
REF = '
A =

    (1,1)          10000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e3)');
REF = '
A =

    (1,1)           1000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e2)');
REF = '
A =

    (1,1)      100

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e1)');
REF = '
A =

    (1,1)       10

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1)');
REF = '
A =

    (1,1)        1

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-1)');
REF =  '
A =

    (1,1)       0.1000

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-2)');
REF =  '
A =

    (1,1)       0.0100

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-3)');
REF = '
A =

    (1,1)      1.0000e-03

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-4)');
REF = '
A =

    (1,1)      1.0000e-04

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-5)');
REF = '
A =

    (1,1)      1.0000e-05

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-6)');
REF =  '
A =

    (1,1)      1.0000e-06

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-7)');
REF = '
A =

    (1,1)      1.0000e-07

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-8)');
REF =  '
A =

    (1,1)      1.0000e-08

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-9)');
REF = '
A =

    (1,1)      1.0000e-09

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-120)');
REF = '
A =

    (1,1)     1.0000e-120

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-999)');
REF =  '
A =

    All zero sparse: 1×1

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse(pi*1e-4)');
REF = '
R =

    (1,1)      3.1416e-04

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse(pi*1e-3)');
REF =  '
R =

    (1,1)       0.0031

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse(-pi*1e-4)');
REF = '
R =

    (1,1)     -3.1416e-04

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse(-pi*1e-3)');
REF = '
R =

    (1,1)      -0.0031

';
assert_isequal(R, REF)
%=============================================================================
A = zeros(4, 4);
A(3, 3) = 1e-3;
R = evalc('R = sparse(A)');
REF = '
R =

    (3,3)      1.0000e-03

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([-1 -20 -4])');
REF = '
R =

    (1,1)       -1
    (1,2)      -20
    (1,3)       -4

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([814.1 1 NaN Inf -Inf])');
REF =  '
R =

    (1,1)     814.1000
    (1,2)       1.0000
    (1,3)          NaN
    (1,4)          Inf
    (1,5)         -Inf

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([7.0408e-05    0.1643])');
REF = '
R =

    (1,1)       0.0001
    (1,2)       0.1643

';
assert_isequal(R, REF)
%=============================================================================
