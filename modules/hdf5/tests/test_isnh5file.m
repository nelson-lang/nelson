%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isnh5file'), 1);
assert_isequal(nargout('isnh5file'), 1);
%=============================================================================
nh5_filename = [modulepath('hdf5', 'tests'), '/', 'h5save_schema_1.nh5'];
assert_istrue(isnh5file(nh5_filename));
%=============================================================================
nh5_filename_ko = [modulepath('hdf5', 'tests'), '/', 'h5save_schema_X.nh5'];
assert_isfalse(isnh5file(nh5_filename_ko));
%=============================================================================
nh5_filename = [modulepath('hdf5', 'tests'), '/', 'test_isnh5file_header.nh5'];
[tf, version, header] = isnh5file(nh5_filename);
assert_isequal(tf, true);
assert_isequal(version{1}, '-v1');
assert_isequal(header{1}, 'Nelson 1.0 NH5-file on Sun Mar 31 17:28:07 2019');
%=============================================================================
nh5_filename_ko = [modulepath('hdf5', 'tests'), '/', 'h5save_schema_X.nh5'];
[tf, version, header] = isnh5file(nh5_filename_ko);
assert_isequal(tf, false);
assert_isequal(version{1}, '');
assert_isequal(header{1}, '');
%=============================================================================
