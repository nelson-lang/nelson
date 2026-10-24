%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--FILE WATCHER REQUIRED-->
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'File watcher service is not available in the portable runtime.');
clear to_to
path_test = [tempdir(), createGUID()];
mkdir(path_test)
addpath(path_test);
asserts.checkerror('to_to()', sprintf(_('Undefined variable or function: %s'), 'to_to'))
M = ["function r = to_to()"; "r = 33;"; "end"];
filewrite([path_test, '/to_to.ext1'], M);
filewrite([path_test, '/to_to.ext2'], M);
filewrite([path_test, '/to_to.m'], M);
filewrite([path_test, '/to_to.ext3'], M);
filewrite([path_test, '/to_to.ext4'], M);
asserts.isequal(to_to(), 33);
clear to_to
rmpath(path_test);
rmdir(path_test, 's');
asserts.checkerror('to_to()', sprintf(_('Undefined variable or function: %s'), 'to_to'))
%=============================================================================
