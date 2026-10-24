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
% Regression: a Python-object method call obj.P.double() reached through a
% classdef dotReference hook must always dispatch to the object method, even
% after (or before) a builtin call of the same name double(obj.P.ndim) in the
% same session. A prior obj.P.double() used to negative-cache the overload
% name '@py/double', which then suppressed the native handle-cast fast path of
% the double() builtin and raised "Check for incorrect argument data type ...".
%=============================================================================
skip_testsuite(isempty(which('pyenv')), 'Python engine is not available.');
%=============================================================================
env = pyenv();
try
  pyrun('import numpy as np');
catch
  skip_testsuite(true, 'Numpy not available')
end
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = PyBuiltinMethodCollision();
% The order matters: 'b' (a py method double()) runs before 'd'/'e' (a builtin
% double()/char() followed by the py method double()). Only when both forms are
% exercised in the same session does the collision surface.
asserts.isequal(o.a, 1);
asserts.isequal(o.b, [0.25 0.25]);
asserts.isequal(o.c, 'f');
asserts.isequal(o.d, [0.25 0.25]);
asserts.isequal(o.e, [0.25 0.25]);
%=============================================================================
% Same collision, freshly built object, method-first order.
o2 = PyBuiltinMethodCollision();
asserts.isequal(o2.b, [0.25 0.25]);
asserts.isequal(o2.d, [0.25 0.25]);
%=============================================================================
