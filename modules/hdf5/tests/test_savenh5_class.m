%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_h5save_file = [tempdir(), 'test_h5save_class.nh5'];
if isfile(test_h5save_file)
  rmfile(test_h5save_file);
end
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
cplx = complexObj(3, 4);
savenh5(test_h5save_file, 'cplx');
%=============================================================================
R = h5readatt(test_h5save_file, '/cplx', 'NELSON_class');
assert_isequal(R, class(cplx));
R = h5readatt(test_h5save_file, '/cplx', 'NELSON_dimensions');
assert_isequal(R, uint64([1; 1]));
R = h5readatt(test_h5save_file, '/cplx', 'NELSON_object');
assert_isequal(R, uint8(1));
R = h5read(test_h5save_file, '/cplx/0');
assert_isequal(R, 3);
R = h5read(test_h5save_file, '/cplx/1');
assert_isequal(R, 4);
R = h5read(test_h5save_file, '/cplx/fieldnames/0');
assert_isequal(R, uint16('r'));
R = h5read(test_h5save_file, '/cplx/fieldnames/1');
assert_isequal(R, uint16('i'));
%=============================================================================

