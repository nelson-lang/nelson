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
R = (uint64(2^53) - 1);
REF = 9007199254740991u;
assert_isequal(R, REF);
%=============================================================================
R = (uint64(2^53) - 1) - (uint64(2^53) - 1000);
REF = uint64(999);
assert_isequal(R, REF);
%=============================================================================
R = (uint64(2^54) - 1) - (uint64(2^54) - 1000);
REF = uint64(999);
assert_isequal(R, REF);
%=============================================================================
R = (uint64(2^57) - 1) - (uint64(2^57) - 1000);
REF = uint64(999);
assert_isequal(R, REF);
%=============================================================================
R = (uint64(2^59) - 1) - (uint64(2^59) - 1000);
REF = uint64(999);
assert_isequal(R, REF);
%=============================================================================
R = (int64(2^53) - 1) - (int64(2^53) - 1000);
REF = int64(999);
assert_isequal(R, REF);
%=============================================================================
R = (int64(2^54) - 1) - (int64(2^54) - 1000);
REF = int64(999);
assert_isequal(R, REF);
%=============================================================================
R = uint8(0) - uint8(127);
REF = uint8(0);
assert_isequal(R, REF);
%=============================================================================
R = uint8(3) - uint8(4);
REF = uint8(0);
assert_isequal(R, REF);
%=============================================================================
R = uint8(4) - uint8(3);
REF = uint8(1);
assert_isequal(R, REF);
%=============================================================================
R = int8(-127) - int8(127);
REF = int8(intmin('int8'));
assert_isequal(R, REF);
%=============================================================================
R = int8(127) - int8(-127);
REF = int8(intmax('int8'));
assert_isequal(R, REF);
%=============================================================================
R = int8(3) - int8(4);
REF = int8(-1);
assert_isequal(R, REF);
%=============================================================================
R = int8(-3) - int8(4);
REF = int8(-7);
assert_isequal(R, REF);
%=============================================================================
R = int8(3) - int8(-4);
REF = int8(7);
assert_isequal(R, REF);
%=============================================================================
R = int8(-3) - int8(-4);
REF = int8(1);
assert_isequal(R, REF);
%=============================================================================
R = intmax('uint8') - intmax('uint8');
REF = intmin('uint8');
assert_isequal(R, REF);
%=============================================================================
