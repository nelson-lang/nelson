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
cmd = ["import sys"; "V = sys.argv"];
R = pyrun(cmd, "V");
asserts.isequal(R.string(), "");
%=============================================================================
R = strtrim(evalc('pyrunfile("test_pyrunfile_1.py")'));
asserts.isequal(R, 'hello Nelson')
%=============================================================================
R = pyrun(cmd, "V");
asserts.isequal(R.char(), '[]');
%=============================================================================
R = pyrunfile("test_pyrunfile_1.py", "greeting");
asserts.isequal(R.char(), 'hello Nelson')
%=============================================================================
R = strtrim(evalc('pyrunfile("test_pyrunfile_2.py ''hello world''");'));
REF = ['greetings from:', char(10), 'test_pyrunfile_2.py', char(10), 'hello world'];
asserts.isequal(R, REF)
%=============================================================================
R = strtrim(evalc('pyrunfile("test_pyrunfile_2.py ''hello world''");'));
REF = ['greetings from:', char(10), 'test_pyrunfile_2.py', char(10), 'hello world'];
asserts.isequal(R, REF)
%=============================================================================
R = pyrunfile("test_pyrunfile_3.py", "L");
asserts.isequal(class(R), 'py.list');
asserts.isequal(R.string(), ["A", "new", "list"]);
%=============================================================================
R = pyrunfile("test_pyrunfile_4.py", "z", 'x', 5, 'y', 2);
asserts.isequal(R, 3);
%=============================================================================
path_test = [tempname(), ' path with spaces'];
mkdir(path_test);
try
  filename = [path_test, '/test_pyrunfile_path_with_spaces.py'];
  filewrite(filename, ["value = 'path with spaces ok'"]);
  R = pyrunfile(filename, "value");
  asserts.isequal(R.char(), 'path with spaces ok');
  filewrite(filename, ["import sys"; "value = sys.argv[1]"]);
  R = pyrunfile([filename, ' ''argument ok'''], "value");
  asserts.isequal(R.char(), 'argument ok');
catch ex
  rmdir(path_test, 's');
  rethrow(ex);
end
rmdir(path_test, 's');
%=============================================================================
