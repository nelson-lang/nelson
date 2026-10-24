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
% pyfunction wraps a Nelson function handle into a Python callable so Python
% code can call it back synchronously into the same (single-threaded) session.
%=============================================================================
% A wrapped handle is a Python callable object.
sq = @(x) x .^ 2;
f = pyfunction(sq);
asserts.isequal(class(f), 'py.builtin_function_or_method');
%=============================================================================
% Scalar callback: Python passes a float, Nelson squares it, result returns.
y = pyrun("r = g(4.0)", "r", "g", f);
asserts.isequal(y, 16);
%=============================================================================
% The same callable can be reused for several calls.
z = pyrun("a = g(2.0); b = g(3.0); r = a + b", "r", "g", f);
asserts.isequal(z, 13);
%=============================================================================
% numpy array argument: the callback receives the array as a Nelson column
% (3x1) and returns element-wise math on it.
add1 = @(v) double(v) + 1;
f2 = pyfunction(add1);
o = pyrun("import numpy as np; o = g(np.array([1.0, 2.0, 3.0]))", "o", "g", f2);
asserts.isequal(double(o)(:), [2; 3; 4]);
%=============================================================================
% Named function handle.
f3 = pyfunction(@sin);
s0 = pyrun("r = g(0.0)", "r", "g", f3);
asserts.isequal(s0, 0);
s = pyrun("r = g(4.0)", "r", "g", f3);
asserts.isapprox(s, sin(4.0), 1e-10);
%=============================================================================
% A Nelson error inside the callback becomes a Python exception whose message
% contains the Nelson message, and Nelson surfaces it cleanly (no crash).
bad = pyfunction(@() error('boom'));
failed = false;
try
  pyrun("r = g()", "r", "g", bad);
catch e
  failed = true;
  asserts.istrue(contains(e.message, 'boom'));
end
asserts.istrue(failed);
%=============================================================================
% pyfunction requires a function handle.
asserts.checkerror('pyfunction(3)', sprintf(_('%s must be a function handle.'), 'Value'));
%=============================================================================
% Calling pyrun from within a callback invoked by Python is rejected with a
% clear error instead of deadlocking (single-threaded, in-process).
nested = pyfunction(@() pyrun("x = 1", "x"));
nested_failed = false;
try
  pyrun("r = g()", "r", "g", nested);
catch e
  nested_failed = true;
end
asserts.istrue(nested_failed);
%=============================================================================
