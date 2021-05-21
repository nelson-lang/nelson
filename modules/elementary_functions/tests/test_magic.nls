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
assert_isequal(nargin('magic'), 1);
assert_isequal(nargout('magic'), 1);
%=============================================================================
R = magic(0);
assert_isequal(R, []);
%=============================================================================
R = magic(-5);
assert_isequal(R, []);
%=============================================================================
R = magic(1);
assert_isequal(R, 1);
%=============================================================================
R = magic(1.2);
assert_isequal(R, 1);
%=============================================================================
R = magic(2);
REF = [1 3;4 2];
assert_isequal(R, REF);
%=============================================================================
R = magic([2, 4]);
REF = [1 3;4 2];
assert_isequal(R, REF);
%=============================================================================
R = magic(5);
REF = [    17    24     1     8    15;
23     5     7    14    16;
 4     6    13    20    22;
10    12    19    21     3;
11    18    25     2     9];
assert_isequal(R, REF);
%=============================================================================
R = magic(5);
REF = [      17     24      1      8     15;
23      5      7     14     16;
 4      6     13     20     22;
10     12     19     21      3;
11     18     25      2      9];
assert_isequal(R, REF);
%=============================================================================
R = magic(single(2));
REF = double([1 3;4 2]);
assert_isequal(R, REF);
%=============================================================================
R = magic(int32(2));
REF = double([1 3;4 2]);
assert_isequal(R, REF);
%=============================================================================
R = magic(4);
REF = [    16     2     3    13;
5    11    10     8;
9     7     6    12;
4    14    15     1];
assert_isequal(R, REF);
%=============================================================================
