%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('whosmat'), 1);
assert_isequal(nargout('whosmat'), 1);
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'),'path'), '/mat/'];
%=============================================================================
s = whosmat([mat_dir, 'test_cell_nest_7.4_GLNX86.mat']);
assert_isequal(s.name, 'testcellnest');
assert_isequal(s.size, [1 2]);
assert_isequal(s.class, 'cell');
assert_isequal(s.global, false);
assert_isequal(s.sparse, false);
assert_isequal(s.complex, false);
assert_isequal(s.nesting, struct('function', '', 'level', 0));
assert_isequal(s.persistent, false);
%=============================================================================
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
savemat([tempdir(), 'test_whosmat-v7.3.mat'], 'A', 'B', 'C', 'D', '-v7.3')
whosmat([tempdir(), 'test_whosmat-v7.3.mat'])
%=============================================================================
