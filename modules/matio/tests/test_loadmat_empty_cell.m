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
clear testemptycell
loadmat([mat_dir, 'test_empty_cell_5.3_SOL2.mat']);
testemptycell_ref = {1.000000 2.000000, [], [] 3.000000};
assert_isequal(testemptycell, testemptycell_ref);
%=============================================================================
clear testemptycell
loadmat([mat_dir, 'test_empty_cell_6.5.1_GLNX86.mat']);
testemptycell_ref = {1.000000 2.000000, [], [] 3.000000};
assert_isequal(testemptycell, testemptycell_ref);
%=============================================================================
clear testemptycell
loadmat([mat_dir, 'test_empty_cell_7.1_GLNX86.mat']);
testemptycell_ref = {1.000000 2.000000, [], [] 3.000000};
assert_isequal(testemptycell, testemptycell_ref);
%=============================================================================
clear testemptycell
loadmat([mat_dir, 'test_empty_cell_7.4_GLNX86.mat']);
testemptycell_ref = {1.000000 2.000000, [], [] 3.000000};
assert_isequal(testemptycell, testemptycell_ref);
%=============================================================================
