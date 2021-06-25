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
mat_dir = [fileparts(nfilename('fullpathext'),'path'), '/mat/'];
%=============================================================================
M_signed = [1   -4    7; -9    6   -3];
M_unsigned = [1   0   7;   0   6   0];
%=============================================================================
clear int8matrix
loadmat([mat_dir, 'test_int8matrix_v6.1_GLNX86.mat']);
assert_isequal(int8matrix, int8(M_signed))
%=============================================================================
clear int8matrix
loadmat([mat_dir, 'test_int8matrix_v7.1_GLNX86.mat']);
assert_isequal(int8matrix, int8(M_signed))
%=============================================================================
clear int16matrix
loadmat([mat_dir, 'test_int16matrix_v6.1_GLNX86.mat']);
assert_isequal(int16matrix, int16(M_signed))
%=============================================================================
clear int16matrix
loadmat([mat_dir, 'test_int16matrix_v7.1_GLNX86.mat']);
assert_isequal(int16matrix, int16(M_signed))
%=============================================================================
clear int32matrix
loadmat([mat_dir, 'test_int32matrix_v6.1_GLNX86.mat']);
assert_isequal(int32matrix, int32(M_signed))
%=============================================================================
clear int32matrix
loadmat([mat_dir, 'test_int32matrix_v7.1_GLNX86.mat']);
assert_isequal(int32matrix, int32(M_signed))
%=============================================================================
clear int64matrix
loadmat([mat_dir, 'test_int64matrix_v6.1_GLNX86.mat']);
assert_isequal(int64matrix, int64(M_signed))
%=============================================================================
clear int64matrix
loadmat([mat_dir, 'test_int64matrix_v7.1_GLNX86.mat']);
assert_isequal(int64matrix, int64(M_signed))
%=============================================================================
clear uint8matrix
loadmat([mat_dir, 'test_uint8matrix_v6.1_GLNX86.mat']);
assert_isequal(uint8matrix, uint8(M_unsigned))
%=============================================================================
clear uint8matrix
loadmat([mat_dir, 'test_uint8matrix_v7.1_GLNX86.mat']);
assert_isequal(uint8matrix, uint8(M_unsigned))
%=============================================================================
clear uint16matrix
loadmat([mat_dir, 'test_uint16matrix_v6.1_GLNX86.mat']);
assert_isequal(uint16matrix, uint16(M_unsigned))
%=============================================================================
clear uint16matrix
loadmat([mat_dir, 'test_uint16matrix_v7.1_GLNX86.mat']);
assert_isequal(uint16matrix, uint16(M_unsigned))
%=============================================================================
clear uint32matrix
loadmat([mat_dir, 'test_uint32matrix_v6.1_GLNX86.mat']);
assert_isequal(uint32matrix, uint32(M_unsigned))
%=============================================================================
clear uint32matrix
loadmat([mat_dir, 'test_uint32matrix_v7.1_GLNX86.mat']);
assert_isequal(uint32matrix, uint32(M_unsigned))
%=============================================================================
clear uint64matrix
loadmat([mat_dir, 'test_uint64matrix_v6.1_GLNX86.mat']);
assert_isequal(uint64matrix, uint64(M_unsigned))
%=============================================================================
clear uint64matrix
loadmat([mat_dir, 'test_uint64matrix_v7.1_GLNX86.mat']);
assert_isequal(uint64matrix, uint64(M_unsigned))
%=============================================================================
