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
format('long');
%=============================================================================
A = [1 2 3];
R = evalc('A');
REF =       '
A =

     1     2     3

';
assert_isequal(R, REF);
%=============================================================================
A = [1 -10 10];
R = evalc('A');
REF =       '
A =

     1   -10    10

';
assert_isequal(R, REF);
%=============================================================================
A = [1 10 100];
R = evalc('A');
REF =   '
A =

     1    10   100

';
assert_isequal(R, REF);
%=============================================================================
A = [1 10 1000];
R = evalc('A');
REF =   '
A =

           1          10        1000

';
assert_isequal(R, REF);
%=============================================================================
A = [1 10 1e5];
R = evalc('A');
REF =    '
A =

           1          10      100000

';
assert_isequal(R, REF);
%=============================================================================
A = [1 10 1e6];
R = evalc('A');
REF =     '
A =

           1          10     1000000

';
assert_isequal(R, REF);
%=============================================================================
A = [1 10 1e7];
R = evalc('A');
REF =     '
A =

           1          10    10000000

';
assert_isequal(R, REF);
%=============================================================================
A = [1 1000 1e8];
R = evalc('A');
REF =   '
A =

           1        1000   100000000

';
assert_isequal(R, REF);
%=============================================================================
A = -[1 1000 1e8];
R = evalc('A');
REF =    '
A =

          -1       -1000  -100000000

';
assert_isequal(R, REF);
%=============================================================================
A = [1 1000 1e9];
R = evalc('A');
REF =  '
A =

   1.0e+09 *

   0.000000001000000   0.000001000000000   1.000000000000000

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = double([1e4 1.1e4 1.2e4])');
REF =  '
A =

       10000       11000       12000

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = double([1e5 1.1e5 1.2e5])');
REF =  '
A =

      100000      110000      120000

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = double([1e6 1.1e6 1.2e6])');
REF =  '
A =

     1000000     1100000     1200000

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = double([1e7 1.1e7 1.2e7])');
REF = '
A =

    10000000    11000000    12000000

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = double([1e8 1.1e8 1.2e8])');
REF = '
A =

   100000000   110000000   120000000

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('A = double([1e9 1.1e9 1.2e9])');
REF =  '
A =

   1.0e+09 *

   1.000000000000000   1.100000000000000   1.200000000000000

';
assert_isequal(R, REF);
%=============================================================================