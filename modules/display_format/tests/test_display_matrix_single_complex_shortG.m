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
A = complex(0, 0);
R = evalc('A = single(A)');
REF = '
A =

  single

     0

';
assert_isequal(R, REF)
%=============================================================================
A = complex(0, 1);
R = evalc('A = single(A)');
REF = '
A =

  single

           0 +          1i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(0, -eps);
R = evalc('A = single(A)');
REF = '
A =

  single

           0 - 2.2204e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(1.8e99, -eps);
R = evalc('A = single(A)');
REF =   '
A =

  single

         Inf - 2.2204e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(2, 3);
R = evalc('A = single(A)');
REF = '
A =

  single

           2 +          3i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(1e2, pi);
R = evalc('A = single(A)');
REF = '
A =

  single

         100 +     3.1416i

';
assert_isequal(R, REF)
%=============================================================================
rng('default')
A = complex(rand(2, 2), 1);
A(2, 2) = NaN;
R = evalc('A = single(A)');
REF = '
A =

  2×2 single matrix

     0.81472 +          1i     0.12699 +          1i
     0.90579 +          1i         NaN +          0i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(complex([6.5574e-04, 6.5574e-04], pi))');
REF =   '
A =

  1×2 single row vector

  0.00065574 +     3.1416i  0.00065574 +     3.1416i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(complex(ones(2,2), pi*1e6))');
REF = '
A =

  2×2 single matrix

           1 + 3.1416e+06i           1 + 3.1416e+06i
           1 + 3.1416e+06i           1 + 3.1416e+06i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(complex(ones(2,2)*1000,pi*1e6))');
REF = '
A =

  2×2 single matrix

        1000 + 3.1416e+06i        1000 + 3.1416e+06i
        1000 + 3.1416e+06i        1000 + 3.1416e+06i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single([0.3729   6.5574e-04    0.1393 0]+ eps * i)');
REF = '
A =

  1×4 single row vector

  Columns 1 through 3

      0.3729 + 2.2204e-16i  0.00065574 + 2.2204e-16i      0.1393 + 2.2204e-16i

  Column 4

           0 + 2.2204e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(eye(3,3)*pi,-Inf);
A(2,2) = pi*i;
R = evalc('A = single(A)');
REF =  '
A =

  3×3 single matrix

      3.1416 -        Infi           0 -        Infi           0 -        Infi
           0 -        Infi           0 +     3.1416i           0 -        Infi
           0 -        Infi           0 -        Infi      3.1416 -        Infi

';
assert_isequal(R, REF)
%=============================================================================
