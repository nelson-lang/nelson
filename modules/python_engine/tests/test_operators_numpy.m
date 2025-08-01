%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
env = pyenv();
[r, msg] = system([env.Executable + " -m pip install numpy --quiet"]);
%=============================================================================
try
  pyrun('import numpy as np');
catch
  skip_testsuite(true, 'Numpy not available')
end
%=============================================================================
N_A = [1, 2, 3; 4, 5, 6; 7, 8, 9];
N_B = [9, 8, 7; 6, 5, 4; 3, 2, 1];
A = pyrun('A = np.asarray(B)','A', 'B', N_A);
B = pyrun('B = np.asarray(C)','B', 'C', N_B);
%=============================================================================
% __pos__
R = +A;
assert_isequal(R, A);
%=============================================================================
% __neg__
R = -A;
assert_isequal(R.double(), -N_A);
%=============================================================================
% __mul__
R = A * B;
assert_isequal(R.double(), N_A .* N_B);
%=============================================================================
% __truediv__
R = A / B;
assert_isapprox(R.double(), N_A ./ N_B, 1e-3);
%=============================================================================
% __pow__
R = A ^ B;
assert_isapprox(R.double(), N_A .^ N_B, 1e-3);
%=============================================================================
% __gt__
R = A > B;
assert_isequal(R.logical(), [false, false, false;false, false, true; true, true, true]);
%=============================================================================
% __ge__
R = A >= B;
assert_isequal(R.logical(), [false, false, false; false, true, true; true, true, true]);
%=============================================================================
% __le__
R = A <= B;
assert_isequal(R.logical(), [true, true, true;true, true, false;false, false, false]);
%=============================================================================
% __ne__
R = A ~= B;
REF = N_A ~= N_B;
assert_isequal(R.logical(), REF);
%=============================================================================
% __lt__
R = A < B;
assert_isequal(R.logical(), [true, true, true;true, false, false;false, false, false]);
%=============================================================================
% __add__
R = A + B;
assert_isequal(R.double(), [10, 10, 10;10, 10, 10;10, 10, 10]);
%=============================================================================
% __sub__
R = A - B;
assert_isequal(R.double(), [-8, -6, -4;-2, 0, 2; 4, 6, 8]);
%=============================================================================
% __eq__
R = (A == B);
assert_isequal(R.logical(), N_A == N_B);
%=============================================================================
% __mod__
R = mod(A, B);
assert_isequal(R.double(), [1, 2, 3;4, 0, 2;1, 0, 0]);
%=============================================================================
