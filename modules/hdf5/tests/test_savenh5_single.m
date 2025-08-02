%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_single.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% single
%=============================================================================
A = single(eye(3, 4));
savenh5(test_h5save_file, 'A');
schema_version = h5readatt(test_h5save_file, '/', 'NELSON_schema');
assert_isequal(schema_version, int32(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
%=============================================================================
A = single(1);
savenh5(test_h5save_file, 'A');
schema_version = h5readatt(test_h5save_file, '/', 'NELSON_schema');
assert_isequal(schema_version, int32(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
%=============================================================================
A = single(eye(3, 4) + i);
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_complex');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
REF = struct();
REF.real = single(eye(3, 4));
REF.imag = single(ones(3, 4));
assert_isequal(R, REF);
%=============================================================================
A = single(1 + 2i);
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_complex');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
REF = struct();
REF.real = single(1);
REF.imag = single(2);
assert_isequal(R, REF);
%=============================================================================
A = single(ones(3, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, single(0));
%=============================================================================
A = single(ones(0, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, single(0));
%=============================================================================
A = single(ones(3, 0, 2, 4));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, single(0));
%=============================================================================
A = single(ones(3, 0, 0, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, single(0));
%=============================================================================
A = single(ones(3, 0, 2, 4) + 4i);
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, single(0));
%=============================================================================
A = single(reshape(1:18,3,2,3));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, A);
%=============================================================================
A = single(reshape(1:18, 3, 2, 3) + 2i);
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'single');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_complex');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
REF = struct();
REF.real = single(reshape(1:18, 3, 2, 3));
REF.imag = single(ones(3, 2, 3) .* 2);
assert_isequal(R, REF);
%=============================================================================
