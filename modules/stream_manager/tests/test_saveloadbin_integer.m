%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
