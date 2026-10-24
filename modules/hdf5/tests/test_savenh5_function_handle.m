%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear all
test_h5save_file = [tempname(), '.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
A = 1;
B = 2;
C = @(x)(x + A) + B;
%=============================================================================
savenh5(test_h5save_file, 'C');
R = h5readatt(test_h5save_file, '/C', 'NELSON_class');
asserts.isequal(R, 'function_handle');
R = h5readatt(test_h5save_file, '/C', 'NELSON_dimensions');
asserts.isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/C', 'NELSON_object');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/C/is_function_handle');
asserts.isequal(R, uint8(0));
R = h5read(test_h5save_file, '/C/function_handle');
asserts.isequal(char(R), '(x+A)+B');
R = h5read(test_h5save_file, '/C/arguments/0');
asserts.isequal(char(R), 'x');
%=============================================================================
clear A
clear B
clear C
R = h5read(test_h5save_file, '/C/definition');
asserts.isequal(char(R), '(x+A)+B');
loadnh5(test_h5save_file)
R = C(3);
REF = 6;
asserts.isequal(R, REF);
asserts.isequal(func2str(C), '@(x)(x+A)+B');
D = @(x) x .* 2 + ... as written
  [x 1];
savenh5(test_h5save_file, 'D');
clear D
loadnh5(test_h5save_file)
asserts.isequal(func2str(D), '@(x)x.*2+[x,1]');
asserts.isequal(D(1), [3 3]);
%=============================================================================
fn2 = str2func('cos');
savenh5(test_h5save_file, 'fn2');
R = h5readatt(test_h5save_file, '/fn2', 'NELSON_class');
asserts.isequal(R, 'function_handle');
R = h5readatt(test_h5save_file, '/fn2', 'NELSON_dimensions');
asserts.isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/fn2', 'NELSON_object');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/fn2/is_function_handle');
asserts.isequal(R, uint8(1));
R = h5read(test_h5save_file, '/fn2/function_handle');
asserts.isequal(R, uint16('cos'));
%=============================================================================
rmfile(test_h5save_file);
%=============================================================================
