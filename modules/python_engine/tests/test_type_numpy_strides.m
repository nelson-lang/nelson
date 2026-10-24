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
% C-order 2-D (default numpy layout)
c = pyrun("c = np.arange(6.0).reshape(2,3)", "c");
asserts.isequal(class(c), 'py.numpy.ndarray')
asserts.isequal(c.double(), [0 1 2; 3 4 5])
%=============================================================================
% F-order 2-D (fast path)
f = pyrun("f = np.asfortranarray(np.arange(6.0).reshape(2,3))", "f");
asserts.isequal(f.double(), [0 1 2; 3 4 5])
%=============================================================================
% C-order 3-D
d3 = pyrun("d3 = np.arange(24.0).reshape(2,3,4)", "d3");
r = d3.double();
asserts.isequal(size(r), [2 3 4])
asserts.isequal(r(1, 2, 3), 6)
asserts.isequal(r(2, 3, 4), 23)
expected = zeros(2, 3, 4);
for i = 1:2
  for j = 1:3
    for k = 1:4
      expected(i, j, k) = (i - 1) * 12 + (j - 1) * 4 + (k - 1);
    end
  end
end
asserts.isequal(r, expected)
%=============================================================================
% F-order 3-D
f3 = pyrun("f3 = np.asfortranarray(np.arange(24.0).reshape(2,3,4))", "f3");
asserts.isequal(f3.double(), expected)
%=============================================================================
% sliced view (non-contiguous, positive strides)
s = pyrun("s = np.arange(12.0).reshape(3,4)[:, ::2]", "s");
asserts.isequal(s.double(), [0 2; 4 6; 8 10])
%=============================================================================
% negative strides
n = pyrun("n = np.arange(6.0)[::-1]", "n");
asserts.isequal(n.double(), [5 4 3 2 1 0])
n2 = pyrun("n2 = np.arange(6.0).reshape(2,3)[::-1, ::-1]", "n2");
asserts.isequal(n2.double(), [5 4 3; 2 1 0])
%=============================================================================
% transposed view (F-contiguous view of a C array)
t = pyrun("t = np.arange(6.0).reshape(2,3).T", "t");
asserts.isequal(t.double(), [0 3; 1 4; 2 5])
%=============================================================================
% 1-D stays a row vector
v = pyrun("v = np.arange(5.0)", "v");
asserts.isequal(v.double(), [0 1 2 3 4])
v2 = pyrun("v2 = np.arange(10.0)[1::3]", "v2");
asserts.isequal(v2.double(), [1 4 7])
%=============================================================================
% 0-D scalar and empty arrays
z = pyrun("z = np.array(3.5)", "z");
asserts.isequal(z.double(), 3.5)
e = pyrun("e = np.zeros((0, 3))", "e");
asserts.isequal(size(e.double()), [0 3])
e2 = pyrun("e2 = np.zeros((2, 0, 4))", "e2");
asserts.isequal(size(e2.double()), [2 0 4])
%=============================================================================
% int32
i2 = pyrun("i2 = np.arange(6, dtype=np.int32).reshape(2,3)", "i2");
asserts.isequal(i2.numeric(), int32([0 1 2; 3 4 5]))
asserts.isequal(i2.int32(), int32([0 1 2; 3 4 5]))
%=============================================================================
% uint8 sliced
u = pyrun("u = np.arange(12, dtype=np.uint8).reshape(3,4)[1:, 1:]", "u");
asserts.isequal(u.numeric(), uint8([5 6 7; 9 10 11]))
%=============================================================================
% bool
b = pyrun("b = np.array([[True, False, True], [False, True, False]])", "b");
asserts.isequal(b.logical(), logical([1 0 1; 0 1 0]))
%=============================================================================
% complex
cz = pyrun("cz = (np.arange(6.0) + 1j * np.arange(6.0, 12.0)).reshape(2,3)", "cz");
asserts.isequal(cz.double(), [0 + 6i 1 + 7i 2 + 8i; 3 + 9i 4 + 10i 5 + 11i])
cz64 = pyrun("cz64 = (np.arange(6.0) + 1j * np.arange(6.0, 12.0)).reshape(2,3).astype(np.complex64)", "cz64");
asserts.isequal(cz64.numeric(), single([0 + 6i 1 + 7i 2 + 8i; 3 + 9i 4 + 10i 5 + 11i]))
%=============================================================================
% round trip Nelson -> Python -> Nelson, including N-D
A = magic(4);
R = pyrun("B = np.array(A)", "B", "A", A);
asserts.isequal(R.double(), A)
A3 = reshape(1:24, [2 3 4]);
R3 = pyrun("B3 = np.array(A3)", "B3", "A3", A3);
asserts.isequal(R3.double(), A3)
R3c = pyrun("B3c = np.ascontiguousarray(A3)", "B3c", "A3", A3);
asserts.isequal(R3c.double(), A3)
%=============================================================================
