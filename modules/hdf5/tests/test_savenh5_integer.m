%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_integer.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% integers
%=============================================================================
A = int8(eye(3, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = int8(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, int8(0));
%=============================================================================
A = uint8(eye(3, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = uint8(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, uint8(0));
%=============================================================================
A = int16(eye(3, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = int16(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, int16(0));
%=============================================================================
A = uint16(eye(3, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = uint16(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, uint16(0));
%=============================================================================
A = int32(eye(3, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = int32(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, int32(0));
%=============================================================================
A = uint32(eye(3, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = uint32(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, uint32(0));
%=============================================================================
A = int64(eye(3, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = int64(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, int64(0));
%=============================================================================
A = uint64(eye(3, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = uint64(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, class(A));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, uint64(0));
%=============================================================================
