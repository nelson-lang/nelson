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
format('longE');
%=============================================================================
A = [1 2 3];
R = evalc('A');
REF =      '
A =

     1     2     3

';
assert_isequal(R, REF);
%=============================================================================
A = [1 -10 10];
R = evalc('A');
REF =   '
A =

     1   -10    10

';
assert_isequal(R, REF);
%=============================================================================
A = [1 10 100];
R = evalc('A');
REF =  '
A =

     1    10   100

';
assert_isequal(R, REF);
%=============================================================================
A = [1 10 1000];
R = evalc('A');
REF = '
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
REF = '
A =

           1          10     1000000

';
assert_isequal(R, REF);
%=============================================================================
A = [1 10 1e7];
R = evalc('A');
REF = '
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
REF =  '
A =

          -1       -1000  -100000000

';
assert_isequal(R, REF);
%=============================================================================
A = [1 1000 1e9];
R = evalc('A');
REF =  '
A =

     1.000000000000000e+00     1.000000000000000e+03     1.000000000000000e+09

';
assert_isequal(R, REF);
%=============================================================================
