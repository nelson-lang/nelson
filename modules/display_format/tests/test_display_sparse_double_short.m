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
format('short')
%=============================================================================
R = evalc('A = sparse([6.5574e-04])');
REF = '
A =

  sparse double

   (1,1)      6.5574e-04

';
asserts.isequal(R, REF)
%=============================================================================
A = sparse([]);
A(1, 1) = 1;
A(1000, 20) = 3;
A(20, 1000) = 4;
R = evalc('A');
REF = '
A =

  1000×1000 sparse double matrix (3 nonzeros)

   (1,1)        1
(1000,20)       3
  (20,1000)     4

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([6.5574e-04, 6.5574e-04])');
REF = '
A =

  1×2 sparse double row vector (2 nonzeros)

   1.0e-03 *

   (1,1)       0.6557
   (1,2)       0.6557

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([0.3729   6.5574e-04    0.1393])');
REF = '
A =

  1×3 sparse double row vector (3 nonzeros)

   (1,1)       0.3729
   (1,2)       0.0007
   (1,3)       0.1393

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([])');
REF = '
A =

  0×0 empty sparse double matrix

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(ones(0,4))');
REF = '
A =

  0×4 empty sparse double matrix

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(10000,5000)');
REF = '
A =

  10000×5000 sparse double matrix

   All zero

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(eps)');
REF = '
A =

  sparse double

   (1,1)      2.2204e-16

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(NaN)');
REF = '
A =

  sparse double

   (1,1)      NaN

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(Inf)');
REF = '
A =

  sparse double

   (1,1)      Inf

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(-Inf)');
REF = '
A =

  sparse double

   (1,1)     -Inf

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-3 1.1e-3 1.2e-3])');
REF = '
A =

  1×3 sparse double row vector (3 nonzeros)

   (1,1)       0.0010
   (1,2)       0.0011
   (1,3)       0.0012

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e-4 1.1e-4 1.2e-4])');
REF = '
A =

  1×3 sparse double row vector (3 nonzeros)

   1.0e-03 *

   (1,1)       0.1000
   (1,2)       0.1100
   (1,3)       0.1200

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([1e4 1.1e4 1.2e4])');
REF = '
A =

  1×3 sparse double row vector (3 nonzeros)

   (1,1)          10000
   (1,2)          11000
   (1,3)          12000

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse([10 10 13])');
REF = '
A =

  1×3 sparse double row vector (3 nonzeros)

   (1,1)       10
   (1,2)       10
   (1,3)       13

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e99)');
REF = '
A =

  sparse double

   (1,1)      1.0000e+99

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e10)');
REF = '
A =

  sparse double

   (1,1)      1.0000e+10

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e9)');
REF = '
A =

  sparse double

   (1,1)      1.0000e+09

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e8)');
REF = '
A =

  sparse double

   (1,1)      100000000

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e7)');
REF = '
A =

  sparse double

   (1,1)       10000000

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e6)');
REF = '
A =

  sparse double

   (1,1)        1000000

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e5)');
REF = '
A =

  sparse double

   (1,1)         100000

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e4)');
REF = '
A =

  sparse double

   (1,1)          10000

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e3)');
REF = '
A =

  sparse double

   (1,1)           1000

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e2)');
REF = '
A =

  sparse double

   (1,1)      100

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e1)');
REF = '
A =

  sparse double

   (1,1)       10

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1)');
REF = '
A =

  sparse double

   (1,1)        1

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-1)');
REF = '
A =

  sparse double

   (1,1)       0.1000

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-2)');
REF = '
A =

  sparse double

   (1,1)       0.0100

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-3)');
REF = '
A =

  sparse double

   (1,1)      1.0000e-03

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-4)');
REF = '
A =

  sparse double

   (1,1)      1.0000e-04

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-5)');
REF = '
A =

  sparse double

   (1,1)      1.0000e-05

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-6)');
REF = '
A =

  sparse double

   (1,1)      1.0000e-06

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-7)');
REF = '
A =

  sparse double

   (1,1)      1.0000e-07

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-8)');
REF = '
A =

  sparse double

   (1,1)      1.0000e-08

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-9)');
REF = '
A =

  sparse double

   (1,1)      1.0000e-09

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-120)');
REF = '
A =

  sparse double

   (1,1)     1.0000e-120

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('A = sparse(1e-999)');
REF = '
A =

  sparse double

   All zero

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('R = sparse(pi*1e-4)');
REF = '
R =

  sparse double

   (1,1)      3.1416e-04

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('R = sparse(pi*1e-3)');
REF = '
R =

  sparse double

   (1,1)       0.0031

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('R = sparse(-pi*1e-4)');
REF = '
R =

  sparse double

   (1,1)     -3.1416e-04

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('R = sparse(-pi*1e-3)');
REF = '
R =

  sparse double

   (1,1)      -0.0031

';
asserts.isequal(R, REF)
%=============================================================================
A = zeros(4, 4);
A(3, 3) = 1e-3;
R = evalc('R = sparse(A)');
REF = '
R =

  4×4 sparse double matrix (1 nonzero)

   (3,3)      1.0000e-03

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([-1 -20 -4])');
REF = '
R =

  1×3 sparse double row vector (3 nonzeros)

   (1,1)       -1
   (1,2)      -20
   (1,3)       -4

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([814.1 1 NaN Inf -Inf])');
REF = '
R =

  1×5 sparse double row vector (5 nonzeros)

   (1,1)     814.1000
   (1,2)       1.0000
   (1,3)          NaN
   (1,4)          Inf
   (1,5)         -Inf

';
asserts.isequal(R, REF)
%=============================================================================
R = evalc('R = sparse([7.0408e-05    0.1643])');
REF = '
R =

  1×2 sparse double row vector (2 nonzeros)

   (1,1)       0.0001
   (1,2)       0.1643

';
asserts.isequal(R, REF)
%=============================================================================
