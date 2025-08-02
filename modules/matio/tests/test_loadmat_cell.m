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
clear testcell
loadmat([mat_dir, 'test_cell_6.1_SOL2.mat']);
testcell_ref{1} = 'This cell contains this string and 3 arrays of increasing length';
testcell_ref{2} = 1;
testcell_ref{3} = [1 2];
testcell_ref{4} = [1 2 3];
assert_isequal(testcell, testcell_ref);
%=============================================================================
clear testcell
loadmat([mat_dir, 'test_cell_6.5.1_GLNX86.mat']);
assert_isequal(testcell, testcell_ref);
%=============================================================================
clear testcell
loadmat([mat_dir, 'test_cell_7.1_GLNX86.mat']);
assert_isequal(testcell, testcell_ref);
%=============================================================================
clear testcell
loadmat([mat_dir, 'test_cell_7.4_GLNX86.mat']);
assert_isequal(testcell, testcell_ref);
%=============================================================================
testcellnest_ref{1} = 1;
testcellnest_ref{2} = {2 3 {4 5}};
clear testcellnest
loadmat([mat_dir, 'test_cell_nest_6.1_SOL2.mat']);
assert_isequal(testcellnest, testcellnest_ref);
%=============================================================================
clear testcellnest
loadmat([mat_dir, 'test_cell_nest_6.5.1_GLNX86.mat']);
assert_isequal(testcellnest, testcellnest_ref);
%=============================================================================
clear testcellnest
loadmat([mat_dir, 'test_cell_nest_7.1_GLNX86.mat']);
assert_isequal(testcellnest, testcellnest_ref);
%=============================================================================
clear testcellnest
loadmat([mat_dir, 'test_cell_nest_7.4_GLNX86.mat']);
assert_isequal(testcellnest, testcellnest_ref);
%=============================================================================
