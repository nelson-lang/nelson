%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_struct.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% struct
%=============================================================================
st(1).a = [1, 1];
st(2).a = [2, 2];
st(1).b = [3; 3];
st(2).b = [4; 4];
savenh5(test_h5save_file, 'st');
R = h5readatt(test_h5save_file, '/st', 'NELSON_class');
assert_isequal(R, 'struct');
R = h5readatt(test_h5save_file, '/st', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 2]));
R = h5read(test_h5save_file, '/st/0');
assert_isequal(R, [1, 1]);
R = h5read(test_h5save_file, '/st/1');
assert_isequal(R, [3; 3]);
R = h5read(test_h5save_file, '/st/2');
assert_isequal(R, [2, 2]);
R = h5read(test_h5save_file, '/st/3');
assert_isequal(R, [4; 4]);
R = h5read(test_h5save_file, '/st/fieldnames/0');
assert_isequal(R, uint16('a'));
R = h5read(test_h5save_file, '/st/fieldnames/1');
assert_isequal(R, uint16('b'));
%=============================================================================
st = struct();
st.a = [1, 1];
st.b = [3; 3];
savenh5(test_h5save_file, 'st');
R = h5readatt(test_h5save_file, '/st', 'NELSON_class');
assert_isequal(R, 'struct');
R = h5readatt(test_h5save_file, '/st', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 1]));
R = h5read(test_h5save_file, '/st/0');
assert_isequal(R, [1, 1]);
R = h5read(test_h5save_file, '/st/1');
assert_isequal(R, [3; 3]);
R = h5read(test_h5save_file, '/st/fieldnames/0');
assert_isequal(R, uint16('a'));
R = h5read(test_h5save_file, '/st/fieldnames/1');
assert_isequal(R, uint16('b'));
%=============================================================================
st = struct();
savenh5(test_h5save_file, 'st');
R = h5readatt(test_h5save_file, '/st', 'NELSON_class');
assert_isequal(R, 'struct');
R = h5readatt(test_h5save_file, '/st', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/st/fieldnames', 'NELSON_class');
assert_isequal(R, 'string');
R = h5readatt(test_h5save_file, '/st/fieldnames', 'NELSON_dimensions');
assert_isequal(R,uint64([1; 0]));
R = h5readatt(test_h5save_file, '/st/fieldnames', 'NELSON_empty');
assert_isequal(R, uint8(1));
%=============================================================================
st = struct([]);
savenh5(test_h5save_file, 'st');
R = h5readatt(test_h5save_file, '/st', 'NELSON_class');
assert_isequal(R, 'struct');
R = h5readatt(test_h5save_file, '/st', 'NELSON_dimensions');
assert_isequal(R, uint64([0; 0]));
R = h5readatt(test_h5save_file, '/st', 'NELSON_empty');
assert_isequal(R, uint8(1));
R = h5readatt(test_h5save_file, '/st/fieldnames', 'NELSON_class');
assert_isequal(R, 'string');
R = h5readatt(test_h5save_file, '/st/fieldnames', 'NELSON_dimensions');
assert_isequal(R,uint64([1; 0]));
R = h5readatt(test_h5save_file, '/st/fieldnames', 'NELSON_empty');
assert_isequal(R, uint8(1));
%=============================================================================
