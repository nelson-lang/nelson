%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_string.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% strings array
%=============================================================================
A = "Nelson";
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'string');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
R = h5readatt(test_h5save_file, '/A/0', 'NELSON_class');
assert_isequal(R, 'char');
R = h5readatt(test_h5save_file, '/A/0', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 6]));
R = h5read(test_h5save_file, '/A/0');
REF = uint16([78     101     108     115     111     110]);
assert_isequal(R, REF);
%=============================================================================
A = ["Nelson" , string(NaN)];
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A/1', 'NELSON_class');
assert_isequal(R, 'double');
R = h5readatt(test_h5save_file, '/A/1', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 1]));
R = h5read(test_h5save_file, '/A/1');
assert_isequal(R, NaN);
%=============================================================================
A = string();
savenh5(test_h5save_file, 'A');
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'string');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/A/0', 'NELSON_class');
assert_isequal(R, 'char');
R = h5readatt(test_h5save_file, '/A/0', 'NELSON_dimensions');
assert_isequal(R, uint64([0; 0]));
R = h5read(test_h5save_file, '/A/0');
assert_isequal(R, uint16(0));
%=============================================================================
