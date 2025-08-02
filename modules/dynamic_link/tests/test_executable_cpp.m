%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc()
end
skip_testsuite(~havecompiler())
%=============================================================================
destinationdir = [tempdir(), 'test_executable_cpp'];
mkdir(destinationdir);
%=============================================================================
currentpath = fileparts(nfilename('fullpathext'));
copyfile([currentpath, '/test_executable_cpp.cpp'], [destinationdir, '/test_executable_cpp.cpp']);
%=============================================================================
[status, message] = dlgeneratemake('executable', destinationdir, ...
'test_executable_cpp', ...
[destinationdir, '/test_executable_cpp.cpp'], ...
[destinationdir, '/']);
if ~status
  error(message);
end
destinationPath = [destinationdir, '/CMakeLists.txt'];
assert_istrue(isfile(destinationPath));
[status, message] = dlmake(destinationdir);
assert_istrue(status);
[R, msg] = unix(['"', destinationdir, '/test_executable_cpp', '"']);
assert_istrue(R == 0);
assert_istrue(startsWith(msg, 'It works with C++!!!'));
%=============================================================================
