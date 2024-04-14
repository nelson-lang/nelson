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
A = magic(6);
B = eye(6, 6);
cmd =["import numpy as np"; "arr = np.array(A) * np.array(B)"];
R = pyrun(cmd, "arr", 'A', A, 'B', B);
assert_isequal(class(R), 'py.numpy.ndarray')
assert_isequal(R.double(), A .* B)
%=============================================================================
cmd =["import numpy as np"; "R = np.array([100, 200, 300], dtype=np.uint16)"];
R = pyrun(cmd, 'R');
assert_isequal(class(R), 'py.numpy.ndarray')
assert_isequal(R.numeric(), uint16([100, 200, 300]));
%=============================================================================
cmd =["import numpy as np"; "R = np.array([1.5, 2.7, 3.9], dtype=np.float32)"];
R = pyrun(cmd, 'R');
assert_isequal(class(R), 'py.numpy.ndarray')
assert_isequal(R.numeric(), single([1.5, 2.7, 3.9]));
%=============================================================================
cmd =["import numpy as np"; "R = np.array([1+2j, 3-4j, 5+6j], dtype=np.complex64)"];
R = pyrun(cmd, 'R');
assert_isequal(class(R), 'py.numpy.ndarray')
assert_isequal(R.numeric(), single([1+2i, 3-4i, 5+6i]));
%=============================================================================
cmd =["import numpy as np"; "R = np.array([True, False, True])"];
R = pyrun(cmd, 'R');
assert_isequal(class(R), 'py.numpy.ndarray')
assert_isequal(R.numeric(), [true, false, true]);
%=============================================================================
cmd =["import numpy as np"; "A = np.array([3, 2, 1, 4])"];
A = pyrun(cmd, 'A');
R = A.argmax();
assert_isequal(class(R), 'py.numpy.int64')
assert_isequal(R.numeric(), int64(R));
%=============================================================================
