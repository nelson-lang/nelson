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
try
  pyrun('import numpy as np');
catch
  skip_testsuite(true, 'Numpy not available')
end
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
[c, maxsize] = computer();
numpy_version = pyrun('ver=np.__version__','ver');

if (maxsize == 2147483647)
  if startswith(numpy_version, '1.')  
    assert_isequal(class(R), 'py.numpy.intc')
  else
    assert_isequal(class(R), 'py.numpy.int32')
  end
  assert_isequal(R.numeric(), int32(R));
else
  assert_isequal(class(R), 'py.numpy.int64')
  if ispc()
    assert_isequal(R.numeric(), int64(R));
  else
    % Welcome in python world linux vs windows
    assert_isequal(R.numeric(), int32(R));
  end
end
%=============================================================================
