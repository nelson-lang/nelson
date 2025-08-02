%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('whomat'), 1);
assert_isequal(nargout('whomat'), 1);
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'),'path'), '/mat/'];
%=============================================================================
c = whomat([mat_dir, 'test_cell_nest_7.4_GLNX86.mat']);
REF = {'testcellnest'};
assert_isequal(c, REF);
%=============================================================================
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
savemat([tempdir(), 'test_whomat-v7.3.mat'], 'A', 'B', 'C', 'D', '-v7.3')
c = whomat([tempdir(), 'test_whomat-v7.3.mat']);
REF = {'A'; 'B'; 'C'; 'D'}
assert_isequal(c, REF);
%=============================================================================
c = whomat([tempdir(), 'test_whomat-v7.3.mat'], 'A', 'C');
REF = {'A'; 'C'}
assert_isequal(c, REF);
%=============================================================================
whomat([tempdir(), 'test_whomat-v7.3.mat'], 'A', 'C');
whomat([tempdir(), 'test_whomat-v7.3.mat']);
%=============================================================================
