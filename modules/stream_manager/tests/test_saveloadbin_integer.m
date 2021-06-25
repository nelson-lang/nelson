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
addpath([fileparts(nfilename('fullpathext'), 'path'), '/loadsavebin']);
%=============================================================================
% nd array int8
A = ones(3, 4, 6);
A = int8(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array uint8
A = ones(3, 4, 6);
A = uint8(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array int16
A = ones(3, 4, 6);
A = int16(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array uint16
A = ones(3, 4, 6);
A = uint16(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array int32
A = ones(3,4,6);
A = int32(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array uint32
A = ones(3, 4, 6);
A = uint32(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array int64
A = ones(3, 4, 6);
A = int64(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% nd array uint64
A = ones(3, 4, 6);
A = uint64(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% uint8
A = ones(3, 4);
A = uint8(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% int8
A = ones(3, 4);
A = int8(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% uint16
A = ones(3, 4);
A = uint16(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% int16
A = ones(3, 4);
A = int16(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% uint32
A = ones(3, 4);
A = uint32(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% int32
A = ones(3, 4);
A = int32(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% uint64
A = ones(3, 4);
A = uint64(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
% int64
A = ones(3, 4);
A = int64(A);
savebin([tempdir(), 'test_saveload_integer.bin'], 'A');
REF = A;
clear A;
loadbin([tempdir(), 'test_saveload_integer.bin']);
assert_isequal(A, REF);
%=============================================================================
