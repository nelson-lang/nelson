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
% A function whose file is removed from a watched directory must stop
% resolving, without an explicit clear or rehash.
% The semicolons matter: without one the assertion returns its verdict into
% ans instead of raising.
clear fw_removed
path_test = [tempdir(), createGUID()];
mkdir(path_test);
addpath(path_test);
M = ["function r = fw_removed()"; "r = 55;"; "end"];
filewrite([path_test, '/fw_removed.m'], M);
asserts.isequal(fw_removed(), 55);
rmfile([path_test, '/fw_removed.m']);
sleep(1);
asserts.checkerror('fw_removed()', sprintf(_('Undefined variable or function: %s'), 'fw_removed'));
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
