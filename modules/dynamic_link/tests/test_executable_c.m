%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--C/C++ COMPILER REQUIRED-->
%=============================================================================
destinationdir = [tempdir(), 'test_executable_c'];
mkdir(destinationdir);
%=============================================================================
currentpath = fileparts(nfilename('fullpathext'));
copyfile([currentpath, '/test_executable_c.c'], [destinationdir, '/test_executable_c.c']);
%=============================================================================
[status, message] = dlgeneratemake('executable', destinationdir, ...
'test_executable_c', ...
[destinationdir, '/test_executable_c.c'], ...
[destinationdir, '/']);
if ~status
  error(message);
end
destinationPath = [destinationdir, '/CMakeLists.txt'];
assert_istrue(isfile(destinationPath));
[status, message] = dlmake(destinationdir);
assert_istrue(status);
[R, msg] = unix([destinationdir, '/test_executable_c']);
assert_istrue(R == 0);
assert_istrue(startsWith(msg, 'It works with C!!!'));
%=============================================================================