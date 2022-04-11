%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_function_handle.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
fn = str2func('cos');
savenh5(test_h5save_file, 'fn');
R = h5readatt(test_h5save_file, '/fn', 'NELSON_class');
assert_isequal(R, 'function_handle');
R = h5readatt(test_h5save_file, '/fn', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/fn', 'NELSON_object');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/fn/0');
assert_isequal(R, uint16('cos'));
R= h5read(test_h5save_file, '/fn/1');
assert_isequal(R, uint16(0));
R = h5read(test_h5save_file, '/fn/fieldnames/0');
assert_isequal(R, uint16('name'));
R = h5read(test_h5save_file, '/fn/fieldnames/1');
assert_isequal(R, uint16('anonymous'));
%=============================================================================
