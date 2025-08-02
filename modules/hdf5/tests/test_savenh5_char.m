%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_char.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% characters array
%=============================================================================
A = 'Nelson is free';
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'char');
R = h5read(test_h5save_file, '/A');
assert_isequal(R, uint16(A));
%=============================================================================
A = ['Nelson'; '  is  '; ' open '; 'source'];
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'char');
R = h5read(test_h5save_file, '/A');
assert_isequal(R, uint16(A));
%=============================================================================
A = char(ones(3, 0));
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'char');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, uint16(0));
%=============================================================================
