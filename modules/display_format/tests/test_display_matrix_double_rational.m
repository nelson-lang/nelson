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
format('rational')
%=============================================================================
A = eps;
R = evalc('A');
REF =  '
A =

   1/4503599627370496

';
assert_isequal(R, REF)
%=============================================================================
A = -eps;
R = evalc('A');
REF =  '
A =

   -1/4503599627370496

';
assert_isequal(R, REF)
%=============================================================================
A = 0.8147;
R = evalc('A');
REF =  '
A =

   787/966

';
assert_isequal(R, REF)
%=============================================================================
A = [pi eps pi];
R = evalc('A');
REF =  '
A =

    355/113          *    355/113

';
assert_isequal(R, REF)
%=============================================================================
A = [0.8147    0.9134    0.2785;
0.9058    0.6324    0.5469;
0.1270    0.0975    0.9575];
R = evalc('A');
REF = '
A =

    787/966    559/612   557/2000
    125/138    203/321    344/629
   127/1000     39/400    383/400

';
assert_isequal(R, REF)
%=============================================================================
A = [0.8147    0.9134    0.2785;
0.9058    0.6324    0.5469;
0.1270    0.0975    0.9575];
B = A;
B(2,2) = -A(2,2);
R = evalc('B');
REF =  '
B =

    787/966    559/612   557/2000
    125/138   -203/321    344/629
   127/1000     39/400    383/400

';
assert_isequal(R, REF)
%=============================================================================
A = [0.8147    0.9134    0.2785;
0.9058    0.6324    0.5469;
0.1270    0.0975    0.9575];
A(2,2) = 3;
R = evalc('A');
REF = '
A =

    787/966    559/612   557/2000
    125/138          3    344/629
   127/1000     39/400    383/400

';
assert_isequal(R, REF)
%=============================================================================
A = [0.8147    0.9134    0.2785;
0.9058    0.6324    0.5469;
0.1270    0.0975    0.9575];
A(2,2) = -3;
R = evalc('A');
REF = '
A =

    787/966    559/612   557/2000
    125/138         -3    344/629
   127/1000     39/400    383/400

';
assert_isequal(R, REF)
%=============================================================================
A = [0.8147    0.9134    0.2785;
0.9058    0.6324    0.5469;
0.1270    0.0975    0.9575];
A(2,2) = 1e8;
R = evalc('A');
REF = '
A =

    787/966    559/612   557/2000
    125/138          *    344/629
   127/1000     39/400    383/400

';
assert_isequal(R, REF)
%=============================================================================
A = [1 eps -eps];
R = evalc('A');
REF =  '
A =

          1          *          *

';
assert_isequal(R, REF)
%=============================================================================
A = [1e6 1e6];
R = evalc('A');
REF =  '
A =

          *          *

';
assert_isequal(R, REF)
%=============================================================================
A = [1e5 1e5];
R = evalc('A');
REF =   '
A =

     100000     100000

';
assert_isequal(R, REF)
%=============================================================================
A = [-1e5 1e5];
R = evalc('A');
REF =  '
A =

          *     100000

';
assert_isequal(R, REF)
%=============================================================================
A = [-1e6 1e5];
R = evalc('A');
REF =  '
A =

          *     100000

';
assert_isequal(R, REF)
%=============================================================================
A = [-1e6 1e5 -NaN];
R = evalc('A');
REF =  '
A =

          *     100000        0/0

';
assert_isequal(R, REF)
%=============================================================================
A = [-1e6 1e5 NaN];
R = evalc('A');
REF =  '
A =

          *     100000        0/0

';
assert_isequal(R, REF)
%=============================================================================
A = [-1e6 1e5 -Inf];
R = evalc('A');
REF =   '
A =

          *     100000       -1/0

';
assert_isequal(R, REF)
%=============================================================================
A = [-1e6 1e5 Inf];
R = evalc('A');
REF =  '
A =

          *     100000        1/0

';
assert_isequal(R, REF)
%=============================================================================
