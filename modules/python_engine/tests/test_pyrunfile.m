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
cmd = ["import sys"; "V = sys.argv"];
R = pyrun(cmd, "V");
assert_isequal(R.string(), "");
%=============================================================================
R = strtrim(evalc('pyrunfile("test_pyrunfile_1.py")'));
assert_isequal(R, 'hello Nelson')
%=============================================================================
R = pyrun(cmd, "V");
assert_isequal(R.char(), '[]');
%=============================================================================
R = pyrunfile("test_pyrunfile_1.py", "greeting");
assert_isequal(R.char(), 'hello Nelson')
%=============================================================================
R = strtrim(evalc('pyrunfile("test_pyrunfile_2.py ''hello world''");'));
REF = ['greetings from:', char(10), 'test_pyrunfile_2.py', char(10), 'hello world'];
assert_isequal(R, REF)
%=============================================================================
R = strtrim(evalc('pyrunfile("test_pyrunfile_2.py ''hello world''");'));
REF = ['greetings from:', char(10), 'test_pyrunfile_2.py', char(10), 'hello world'];
assert_isequal(R, REF)
%=============================================================================
R = pyrunfile("test_pyrunfile_3.py", "L");
assert_isequal(class(R), 'py.list');
assert_isequal(R.string(), ["A", "new", "list"]);
%=============================================================================
R = pyrunfile("test_pyrunfile_4.py", "z", 'x', 5, 'y', 2);
assert_isequal(R, 3);
%=============================================================================
