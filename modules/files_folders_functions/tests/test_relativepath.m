%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
__dir1  = [tempdir(), 'dir1'];
__dir11 = [tempdir(), 'dir1/dir1.1'];
__dir12 = [tempdir(), 'dir1/dir1.2'];
__file1  = [__dir1, '/file1.txt'];
__file11 = [__dir11,'/file11.txt'];
__file12 =  [__dir12,'/file12.txt'];
%=============================================================================
assert_isequal(relativepath(__dir1, __file1), './file1.txt');
assert_isequal(relativepath(__dir1, __file11), './dir1.1/file11.txt');
assert_isequal(relativepath(__dir1, __file12), './dir1.2/file12.txt');
assert_isequal(relativepath(__dir11, __file11), './file11.txt');
assert_isequal(relativepath(__dir12, __file12), './file12.txt');
%=============================================================================

