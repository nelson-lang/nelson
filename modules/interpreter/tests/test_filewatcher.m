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
clear to_to
path_test = [tempdir(), createGUID()];
mkdir(path_test)
addpath(path_test);
assert_checkerror('to_to()', [_('Undefined variable or function:'), ' ', 'to_to'])
M = ["function r = to_to()"; "r = 33;"; "end"];
filewrite([path_test, '/to_to.ext1'], M);
filewrite([path_test, '/to_to.ext2'], M);
filewrite([path_test, '/to_to.m'], M);
filewrite([path_test, '/to_to.ext3'], M);
filewrite([path_test, '/to_to.ext4'], M);
assert_isequal(to_to(), 33);
clear to_to
rmpath(path_test);
rmdir(path_test, 's');
assert_checkerror('to_to()', [_('Undefined variable or function:'), ' ', 'to_to'])
%=============================================================================
