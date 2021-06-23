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
assert_isequal(nargin('gcd'), 2);
assert_isequal(nargout('gcd'), 1);
%=============================================================================
R = gcd([], []);
REF = [];
assert_isequal(R, REF);
%=============================================================================
R = gcd(ones(3, 0), 3);
REF = ones(3, 0);
assert_isequal(R, REF);
%=============================================================================
R = gcd(3, ones(3, 0));
REF = ones(3, 0);
assert_isequal(R, REF);
%=============================================================================
R = gcd([6 10;12 15], [9 12; 16 20]);
REF = [3     2;4     5];
assert_isequal(R, REF);
%=============================================================================
A = [-5 17; 10 0];
B = [-15 3; 100 0];
R = gcd(A, B);
REF = [ 5     1;10     0];
assert_isequal(R, REF);
%=============================================================================
R = gcd([200, 300, 50, 35], 5);
REF = [5 5 5 5];
assert_isequal(R, REF);
%=============================================================================
R = gcd(int16([200, 300, 50, 35]), 5);
REF = int16([5 5 5 5]);
assert_isequal(R, REF);
%=============================================================================
R = gcd(uint64([200, 300, 50, 35]), 5);
REF = uint64([5 5 5 5]);
assert_isequal(R, REF);
%=============================================================================
R = gcd(int16([100 -30 200]), int16([20 15 9]));
REF = int16([20   15    1]);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('gcd([18-i, -29+3i], -3-4i)', _('Inputs must be real integers.'))
%=============================================================================
