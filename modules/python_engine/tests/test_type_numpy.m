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
try
  pyrun('import numpy as np');
catch
  skip_testsuite(true, 'Numpy not available')
end
%=============================================================================
A = magic(6);
B = eye(6, 6);
cmd = ["import numpy as np"; "arr = np.array(A) * np.array(B)"];
R = pyrun(cmd, "arr", 'A', A, 'B', B);
asserts.isequal(class(R), 'py.numpy.ndarray')
asserts.isequal(R.double(), A .* B)
%=============================================================================
cmd = ["import numpy as np"; "R = np.array([100, 200, 300], dtype=np.uint16)"];
R = pyrun(cmd, 'R');
asserts.isequal(class(R), 'py.numpy.ndarray')
asserts.isequal(R.numeric(), uint16([100, 200, 300]));
%=============================================================================
cmd = ["import numpy as np"; "R = np.array([1.5, 2.7, 3.9], dtype=np.float32)"];
R = pyrun(cmd, 'R');
asserts.isequal(class(R), 'py.numpy.ndarray')
asserts.isequal(R.numeric(), single([1.5, 2.7, 3.9]));
%=============================================================================
cmd = ["import numpy as np"; "R = np.array([1+2j, 3-4j, 5+6j], dtype=np.complex64)"];
R = pyrun(cmd, 'R');
asserts.isequal(class(R), 'py.numpy.ndarray')
asserts.isequal(R.numeric(), single([1 + 2i, 3 - 4i, 5 + 6i]));
%=============================================================================
cmd = ["import numpy as np"; "R = np.array([True, False, True])"];
R = pyrun(cmd, 'R');
asserts.isequal(class(R), 'py.numpy.ndarray')
asserts.isequal(R.numeric(), [true, false, true]);
%=============================================================================
cmd = ["import numpy as np"; "A = np.array([3, 2, 1, 4])"];
A = pyrun(cmd, 'A');
R = A.argmax();
[c, maxsize] = computer();
numpy_version = pyrun('ver=np.__version__', 'ver');

if (maxsize == 2147483647)
  if startswith(numpy_version, '1.')
    asserts.isequal(class(R), 'py.numpy.intc')
  else
    asserts.isequal(class(R), 'py.numpy.int32')
  end
  asserts.isequal(R.numeric(), int32(R));
else
  asserts.isequal(class(R), 'py.numpy.int64')
  asserts.isequal(R.numeric(), int64(R));
end
%=============================================================================
R = pyrun('R = np.array([1 << 40, -(1 << 40)], dtype=np.int64)', 'R');
asserts.isequal(R.numeric(), int64([2^40, -2^40]));
R = pyrun('R = np.array([1 << 63, (1 << 64) - 1], dtype=np.uint64)', 'R');
asserts.isequal(R.numeric(), [uint64(2^63), intmax('uint64')]);
%=============================================================================
