%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
