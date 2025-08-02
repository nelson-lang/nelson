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
try
    pyrun('import numpy as np');
  catch
      skip_testsuite(true, 'Numpy not available')
  end
%=============================================================================
cmd =["import numpy as np"; "A = np.array([3, 2, 1, 4])"];
A = pyrun(cmd, 'A')
isbuiltin('argsort')
ismacro('argsort')
ismethod(A, 'argsort')
R = argsort(A)
assert_isequal(class(R), 'py.numpy.ndarray')

