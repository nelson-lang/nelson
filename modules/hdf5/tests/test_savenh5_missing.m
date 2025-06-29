%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_missing.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
% missing
%=============================================================================
A = missing;
savenh5(test_h5save_file, 'A');
assert_istrue(isfile(test_h5save_file));
schema_version = h5readatt(test_h5save_file, '/', 'NELSON_schema');
assert_isequal(schema_version, int32(1));
R = h5read(test_h5save_file, '/A');
assert_isequal(R, 0);
R = h5readatt(test_h5save_file, '/A', 'NELSON_class');
assert_isequal(R, 'missing');
R = h5readatt(test_h5save_file, '/A', 'NELSON_dimensions');
assert_isequal(R, uint64(size(A)'));
%=============================================================================
B = repmat(missing, 3, 2);
savenh5(test_h5save_file, 'B');
assert_istrue(isfile(test_h5save_file));
schema_version = h5readatt(test_h5save_file, '/', 'NELSON_schema');
assert_isequal(schema_version, int32(1));
R = h5read(test_h5save_file, '/B');
R = h5readatt(test_h5save_file, '/B', 'NELSON_class');
assert_isequal(R, 'missing');
R = h5readatt(test_h5save_file, '/B', 'NELSON_dimensions');
assert_isequal(R, uint64(size(B)'));
%=============================================================================
