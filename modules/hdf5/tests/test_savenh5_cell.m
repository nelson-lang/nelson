%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_cell.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% cell
%=============================================================================
A = {int8(1), uint16(2); int32(3), uint64(4)};
savenh5(test_h5save_file, 'A');
assert_istrue(isfile(test_h5save_file));
schema_version = h5readatt(test_h5save_file, '/', 'NELSON_schema');
assert_isequal(schema_version, int32(1));
R = h5read(test_h5save_file, '/A/0');
assert_isequal(R, A{1});
R = h5read(test_h5save_file, '/A/1');
assert_isequal(R, A{2});
R = h5read(test_h5save_file, '/A/2');
assert_isequal(R, A{3});
R = h5read(test_h5save_file, '/A/3');
assert_isequal(R, A{4});
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'cell');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
%=============================================================================
B = {A, 3, single(9); 1, i, true};
savenh5(test_h5save_file, 'B');
R = h5readatt(test_h5save_file, '/B', 'NELSON_class');
assert_isequal(R, 'cell');
R = h5readatt(test_h5save_file, '/B', 'NELSON_dimensions');
assert_isequal(R, uint64(size(B)'));
R = h5readatt(test_h5save_file, '/B/0', 'NELSON_class');
assert_isequal(R, 'cell');
R = h5read(test_h5save_file, '/B/0/0');
assert_isequal(R, A{1});
R = h5read(test_h5save_file, '/B/0/1');
assert_isequal(R, A{2});
R = h5read(test_h5save_file, '/B/0/2');
assert_isequal(R, A{3});
R = h5read(test_h5save_file, '/B/0/3');
assert_isequal(R, A{4});
R = h5read(test_h5save_file, '/B/1');
assert_isequal(R, B{2});
R = h5read(test_h5save_file, '/B/2');
assert_isequal(R, B{3});
R = h5read(test_h5save_file, '/B/3');
REF = struct();
REF.real = 0;
REF.imag = 1;
assert_isequal(R, REF);
R = h5read(test_h5save_file, '/B/4');
assert_isequal(R, single(9));
R = h5read(test_h5save_file, '/B/5');
assert_isequal(R, uint8(1));
%=============================================================================
C = {B, A; A, {B, A}};
savenh5(test_h5save_file, 'C');
R = h5readatt(test_h5save_file, '/C', 'NELSON_class');
assert_isequal(R, 'cell');
R = h5readatt(test_h5save_file, '/C/0', 'NELSON_class');
assert_isequal(R, 'cell');
R = h5readatt(test_h5save_file, '/C/0/0', 'NELSON_class');
assert_isequal(R, 'cell');
%=============================================================================
D = {};
savenh5(test_h5save_file, 'D');
R = h5readatt(test_h5save_file, '/D', 'NELSON_class');
assert_isequal(R, 'cell');
R = h5readatt(test_h5save_file, '/D', 'NELSON_dimensions');
assert_isequal(R, uint64(size(D)'));
R = h5readatt(test_h5save_file, '/D', 'NELSON_empty');
assert_isequal(R, uint8(1));
%=============================================================================
E = cell(3, 4, 5);
savenh5(test_h5save_file, 'E');
R = h5readatt(test_h5save_file, '/E', 'NELSON_class');
assert_isequal(R, 'cell');
R = h5read(test_h5save_file, '/E/59');
assert_isequal(R, 0);
for k = [0:59]
  R = h5readatt(test_h5save_file, ['/E/', int2str(k)], 'NELSON_class');
  assert_isequal(R, 'double');
  R = h5readatt(test_h5save_file, ['/E/', int2str(k)], 'NELSON_dimensions');
  assert_isequal(R, uint64([0; 0]));
  R = h5readatt(test_h5save_file, ['/E/', int2str(k)], 'NELSON_empty');
  assert_isequal(R, uint8(1));
end
%=============================================================================
