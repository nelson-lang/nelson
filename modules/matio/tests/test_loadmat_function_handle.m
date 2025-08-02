%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'),'path'), '/mat/'];
%=============================================================================
loadmat([mat_dir, 'test_function_handle.mat']);
assert_isequal(a, -3.9);
assert_isequal(b, 52);
assert_isequal(c, 0);
assert_isequal(parabola, struct());
assert_isequal(nCf, struct());
assert_isequal(sqr, struct());
assert_isequal(lastwarn(), [_('Cannot read variable:'), ' ', 'nCf']);
%=============================================================================
