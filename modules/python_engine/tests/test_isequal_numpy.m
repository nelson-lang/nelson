%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
env = pyenv();
[r, msg] = system([env.Executable + " -m pip install numpy"]);
%=============================================================================
try
  pyrun('import numpy as np');
catch
    skip_testsuit(true, 'Numpy not available')
end
%=============================================================================
N_A = [1, 2, 3; 4, 5, 6; 7, 8, 9];
N_B = [9, 8, 7; 6, 5, 4; 3, 2, 1];
N_C = [1, 2, 3; 4, 5, 6; 7, 8, 9];
A = pyrun('A = np.asarray(A)','A', 'A', N_A);
B = pyrun('B = np.asarray(B)','B', 'B', N_B);
C = pyrun('C = np.asarray(C)','C', 'C', N_C);
%=============================================================================
assert_istrue(isequal(A, A))
assert_isfalse(isequal(A, B))
assert_istrue(isequal(A, C))
