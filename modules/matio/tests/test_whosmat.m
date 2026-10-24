%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('whosmat'), 1);
asserts.isequal(nargout('whosmat'), 1);
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'), 'path'), '/mat/'];
%=============================================================================
s = whosmat([mat_dir, 'test_cell_nest_7.4_GLNX86.mat']);
asserts.isequal(s.name, 'testcellnest');
asserts.isequal(s.size, [1 2]);
asserts.isequal(s.class, 'cell');
asserts.isequal(s.global, false);
asserts.isequal(s.sparse, false);
asserts.isequal(s.complex, false);
asserts.isequal(s.nesting, struct('function', '', 'level', 0));
asserts.isequal(s.persistent, false);
%=============================================================================
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
mat_file = [tempname(), '.mat'];
savemat(mat_file, 'A', 'B', 'C', 'D', '-v7.3')
whosmat(mat_file)
rmfile(mat_file);
%=============================================================================
