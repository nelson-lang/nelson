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
R = [11 21 31] * [11;21;31];
REF = 1523;
assert_isequal(R, REF);
%=============================================================================
R = [11;21;31] * [11 21 31];
REF = [121.0000   231.0000   341.0000;
231.0000   441.0000   651.0000;
341.0000   651.0000   961.0000];
assert_isequal(R, REF);
%=============================================================================
M = [11 21;31  41];
R = M * M;
REF = [772 1092; 1612 2332];
assert_isequal(R, REF);
%=============================================================================
R = [11 21 31] * 2;
REF = [22 42 62];
assert_isequal(R, REF);
%=============================================================================
R = [11; 21; 31] * 2;
REF = [22; 42; 62];
assert_isequal(R, REF);
%=============================================================================
R = [11 21i 31] * [11;21;31];
REF = 1082 + 441i;
assert_isequal(R, REF);
%=============================================================================
R = [11;21i;31] * [11 21 31];
REF = [   121.0000   231.0000   341.0000;
0.0000 + 231.0000i     0.0000 + 441.0000i     0.0000 + 651.0000i;
341.0000   651.0000   961.0000];
assert_isequal(R, REF);
%=============================================================================
M = [11 21i;  31  41];
R = M * M;
REF = [   121.0000 + 651.0000i,  0.0000+1092.0000i;
1612.0000,  1681.0000+651.0000i];
assert_isequal(R, REF);
%=============================================================================
R = [1 2i 3] * 231;
REF = [231 462i 693];
assert_isequal(R, REF);
%=============================================================================
R = [13; 23i; 31] * 242;
REF = [3146; 5566i; 7502];
assert_isequal(R, REF);
%=============================================================================
M = [11 21 31];
N = [11 21 31 41];
assert_checkerror('M * N', [_('Size mismatch on arguments to arithmetic operator '), '*']);
%=============================================================================
M = ones(3, 3, 3);
assert_checkerror('M * M', _('Wrong size for input arguments: 2D matrix expected.'));
%=============================================================================
A = 1:2;
B = (1:3)';
assert_checkerror('R = A * B', [_('Size mismatch on arguments to arithmetic operator '), '*']);
%=============================================================================
A = 1:2;
B = (1:3)';
R = B * A;
REF = [    1     2;2     4;3     6];
assert_isequal(R, REF);
%=============================================================================
R = intmax('int32') * intmax('int32');
REF = intmax('int32');
assert_isequal(R, REF);