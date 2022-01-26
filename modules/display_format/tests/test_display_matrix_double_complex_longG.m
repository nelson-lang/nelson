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
format('longG')
%=============================================================================
A = complex(1.8e99, -eps);
R = evalc('A');
REF = '
A =

                    1.8e+99 -  2.22044604925031e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(2, 3);
R = evalc('A');
REF = '
A =

                          2 +                     3i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(1e2, pi);
R = evalc('A');
REF = '
A =

                        100 +      3.14159265358979i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(rand(2, 2), 1);
A(2, 2) = NaN;
R = evalc('A');
REF =  '
A =

  Column 1

          0.814723691903055 +                     1i
           0.90579193411395 +                     1i

  Column 2

          0.126986811868846 +                     1i
                        NaN +                     0i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = complex([6.5574e-04, 6.5574e-04], pi)');
REF = '
A =

  Column 1

                 0.00065574 +      3.14159265358979i

  Column 2

                 0.00065574 +      3.14159265358979i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = complex(ones(2,2), pi*1e6)');
REF = '
A =

  Column 1

                          1 +      3141592.65358979i
                          1 +      3141592.65358979i

  Column 2

                          1 +      3141592.65358979i
                          1 +      3141592.65358979i

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
R = evalc('A = [0.3729   6.5574e-04    0.1393]+ eps * i');
REF = '
A =

  Column 1

                     0.3729 +  2.22044604925031e-16i

  Column 2

                 0.00065574 +  2.22044604925031e-16i

  Column 3

                     0.1393 +  2.22044604925031e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(eye(3,3)*pi,-Inf);
A(2,2) = pi*i;
R = evalc('A');
REF =  '
A =

  Column 1

           3.14159265358979 -                   Infi
                          0 -                   Infi
                          0 -                   Infi

  Column 2

                          0 -                   Infi
                          0 +      3.14159265358979i
                          0 -                   Infi

  Column 3

                          0 -                   Infi
                          0 -                   Infi
           3.14159265358979 -                   Infi

';
assert_isequal(R, REF)
%=============================================================================
