%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rng('default')
format('longG')
%=============================================================================
A = single(complex(eps, -eps));
R = evalc('A');
REF = '
A =

  single

  2.220446e-16 - 2.220446e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = single(complex(pi, -pi));
R = evalc('A');
REF = '
A =

  single

      3.141593 -     3.141593i

';
assert_isequal(R, REF)
%=============================================================================
A = single(complex(1.8e99, -eps));
R = evalc('A');
REF = '
A =

  single

           Inf - 2.220446e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = single(complex(2, 3));
R = evalc('A');
REF = '
A =

  single

             2 +            3i

';
assert_isequal(R, REF)
%=============================================================================
A = single(complex(1e2, pi));
R = evalc('A');
REF = '
A =

  single

           100 +     3.141593i

';
assert_isequal(R, REF)
%=============================================================================
A = single(complex(rand(2, 2), 1));
A(2, 2) = NaN;
R = evalc('A');
REF = '
A =

  2×2 single matrix

     0.8147237 +            1i     0.1269868 +            1i
     0.9057919 +            1i           NaN +            0i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(complex([6.5574e-04, 6.5574e-04], pi))');
REF = '
A =

  1×2 single row vector

    0.00065574 +     3.141593i    0.00065574 +     3.141593i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(complex(ones(2,2), pi*1e6))');
REF = '
A =

  2×2 single matrix

             1 +      3141593i             1 +      3141593i
             1 +      3141593i             1 +      3141593i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = complex(ones(2,2)*1000,pi*1e6)');
REF =  '
A =

  Column 1

                       1000 +      3141592.65358979i
                       1000 +      3141592.65358979i

  Column 2

                       1000 +      3141592.65358979i
                       1000 +      3141592.65358979i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single([0.3729   6.5574e-04    0.1393]+ eps * i)');
REF =  '
A =

  1×3 single row vector

  Columns 1 through 2

        0.3729 + 2.220446e-16i    0.00065574 + 2.220446e-16i

  Column 3

        0.1393 + 2.220446e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(eye(3,3)*pi,-Inf);
A(2,2) = pi*i;
R = evalc('A = single(A)');
REF = '
A =

  3×3 single matrix

  Columns 1 through 2

      3.141593 -          Infi             0 -          Infi
             0 -          Infi             0 +     3.141593i
             0 -          Infi             0 -          Infi

  Column 3

             0 -          Infi
             0 -          Infi
      3.141593 -          Infi

' ;
assert_isequal(R, REF)
%=============================================================================
