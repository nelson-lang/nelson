%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('whomat'), 1);
asserts.isequal(nargout('whomat'), 1);
%=============================================================================
mat_dir = [fileparts(nfilename('fullpathext'), 'path'), '/mat/'];
%=============================================================================
c = whomat([mat_dir, 'test_cell_nest_7.4_GLNX86.mat']);
REF = {'testcellnest'};
asserts.isequal(c, REF);
%=============================================================================
A = ones(3, 4);
B = 'Nelson';
C = sparse(true);
D = sparse(3i);
mat_file = [tempname(), '.mat'];
savemat(mat_file, 'A', 'B', 'C', 'D', '-v7.3')
c = whomat(mat_file);
REF = {'A'; 'B'; 'C'; 'D'}
asserts.isequal(c, REF);
%=============================================================================
c = whomat(mat_file, 'A', 'C');
REF = {'A'; 'C'}
asserts.isequal(c, REF);
%=============================================================================
whomat(mat_file, 'A', 'C');
whomat(mat_file);
rmfile(mat_file);
%=============================================================================
