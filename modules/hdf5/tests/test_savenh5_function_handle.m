%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear all
test_h5save_file = [tempdir(), 'test_h5save_function_handle.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
A = 1;
B = 2;
C = @(x) (x + A) + B;
%=============================================================================
savenh5(test_h5save_file, 'C');
R = h5readatt(test_h5save_file, '/C', 'NELSON_class');
assert_isequal(R, 'function_handle');
R = h5readatt(test_h5save_file, '/C', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/C', 'NELSON_object');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/C/is_function_handle');
assert_isequal(R, uint8(0));
R = h5read(test_h5save_file, '/C/function_handle');
assert_isequal(char(R), '(x+A)+B');
R = h5read(test_h5save_file, '/C/arguments/0');
assert_isequal(char(R), 'x');
%=============================================================================
clear A
clear B
clear C
loadnh5(test_h5save_file)
R = C(3);
REF = 6;
assert_isequal(R, REF);
%=============================================================================
fn2 = str2func('cos');
savenh5(test_h5save_file, 'fn2');
R = h5readatt(test_h5save_file, '/fn2', 'NELSON_class');
assert_isequal(R, 'function_handle');
R = h5readatt(test_h5save_file, '/fn2', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/fn2', 'NELSON_object');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/fn2/is_function_handle');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/fn2/function_handle');
assert_isequal(R, uint16('cos'));
%=============================================================================
