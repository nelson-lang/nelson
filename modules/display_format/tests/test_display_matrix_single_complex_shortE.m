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

  0.0000e+00 +1.0000e+00i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(0, -eps);
R = evalc('A = single(A)');
REF = '
A =

  single

  0.0000e+00 -2.2204e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(1.8e99, -eps);
R = evalc('A = single(A)');
REF = '
A =

  single

         Inf -2.2204e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(2, 3);
R = evalc('A = single(A)');
REF = '
A =

  single

  2.0000e+00 +3.0000e+00i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(1e2, pi);
R = evalc('A = single(A)');
REF = '
A =

  single

  1.0000e+02 +3.1416e+00i

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

  8.1472e-01 +1.0000e+00i  1.2699e-01 +1.0000e+00i
  9.0579e-01 +1.0000e+00i         NaN +0.0000e+00i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(complex([6.5574e-04, 6.5574e-04], pi))');
REF =  '
A =

  1×2 single row vector

  6.5574e-04 +3.1416e+00i  6.5574e-04 +3.1416e+00i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(complex(ones(2,2), pi*1e6))');
REF = '
A =

  2×2 single matrix

  1.0000e+00 +3.1416e+06i  1.0000e+00 +3.1416e+06i
  1.0000e+00 +3.1416e+06i  1.0000e+00 +3.1416e+06i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single(complex(ones(2,2)*1000,pi*1e6))');
REF = '
A =

  2×2 single matrix

  1.0000e+03 +3.1416e+06i  1.0000e+03 +3.1416e+06i
  1.0000e+03 +3.1416e+06i  1.0000e+03 +3.1416e+06i

';
assert_isequal(R, REF)
%=============================================================================
R = evalc('A = single([0.3729   6.5574e-04    0.1393 0]+ eps * i)');
REF = '
A =

  1×4 single row vector

  Columns 1 through 3

  3.7290e-01 +2.2204e-16i  6.5574e-04 +2.2204e-16i  1.3930e-01 +2.2204e-16i

  Columns 4

  0.0000e+00 +2.2204e-16i

';
assert_isequal(R, REF)
%=============================================================================
A = complex(eye(3,3)*pi,-Inf);
A(2,2) = pi*i;
R = evalc('A= single(A)');
REF =  '
A =

  3×3 single matrix

  3.1416e+00 -       Infi  0.0000e+00 -       Infi  0.0000e+00 -       Infi
  0.0000e+00 -       Infi  0.0000e+00 +3.1416e+00i  0.0000e+00 -       Infi
  0.0000e+00 -       Infi  0.0000e+00 -       Infi  3.1416e+00 -       Infi

';
assert_isequal(R, REF)
%=============================================================================
