%=============================================================================
% Copyright (c) 2016+2018 Allan CORNET (Nelson)
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
R = (uint64(2^53) + 1) + (uint64(2^53) + 1000);
REF = 18014398509482985u;
assert_isequal(R, REF);
%=============================================================================
R = (uint64(2^54) + 1) + (uint64(2^54) + 1000);
REF = 36028797018964969u;
assert_isequal(R, REF);
%=============================================================================
R = (uint64(2^57) + 1) + (uint64(2^57) + 1000);
REF = 288230376151712745u;
assert_isequal(R, REF);
%=============================================================================
R = (uint64(2^59) + 1) + (uint64(2^59) + 1000);
REF = 1152921504606847977u;
assert_isequal(R, REF);
%=============================================================================
R = (int64(2^53) + 1) + (int64(2^53) + 1000);
REF = int64(18014398509482985u);
assert_isequal(R, REF);
%=============================================================================
R = (int64(2^54) + 1) + (int64(2^54) + 1000);
REF = int64(36028797018964969u);
assert_isequal(R, REF);
%=============================================================================
R = int8(127) + int8(127);
REF = int8(127);
assert_isequal(R, REF);
%=============================================================================
R = uint64(intmax('uint64')) + uint64(intmax('uint64'));
REF = uint64(intmax('uint64'));
assert_isequal(R, REF);
%=============================================================================
R = int8(-3) + int8(-4);
REF = int8(-7);
assert_isequal(R, REF);
%=============================================================================
R = int32(2) + int32([1 2 3]);
REF = int32([3 4 5]);
assert_isequal(R, REF);
%=============================================================================
R = int32([1 2 3])+ int32(2);
REF = int32([3 4 5]);
assert_isequal(R, REF);
%=============================================================================
