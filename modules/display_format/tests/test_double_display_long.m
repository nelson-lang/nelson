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
% <--ENGLISH IMPOSED-->
%=============================================================================
format long
%==============================================================================
R = evalc('A = eps');
REF =  '
A =

     2.220446049250313e-16

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = pi');
REF = '
A =

   3.141592653589793

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = 3e8');
REF = '
A =

   300000000

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = 3e9');
REF = '
A =

     3.000000000000000e+09

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = -3e8');
REF = '
A =

  -300000000

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = -3e9');
REF = '
A =

    -3.000000000000000e+09

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = 1.2345e-6');
REF = '
A =

     1.234500000000000e-06

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = inv(3000)');
REF = '
A =

     3.333333333333333e-04

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = [4*inv(3) 1.2345e-6]');
REF =  '
A =

   1.333333333333333   0.000001234500000

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = 1.2345e-6');
REF = '
A =

     1.234500000000000e-06

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = [1.2345e-6 1.2345e-6]');
REF = '
A =

   1.0e-05 *

   0.123450000000000   0.123450000000000

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = [1.2345e-6 1.2345e-6 3]');
REF = '
A =

   0.000001234500000   0.000001234500000   3.000000000000000

';
%==============================================================================
R = evalc('A = [1 3e8]');
REF = '
A =

           1   300000000

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = [1 3e9]');
REF = '
A =

   1.0e+09 *

   0.000000001000000   3.000000000000000

';
assert_isequal(R, REF)
%==============================================================================
R = evalc('A = [inf 3 pi]');
REF = '
A =

                 Inf   3.000000000000000   3.141592653589793

';
assert_isequal(R, REF)
%==============================================================================