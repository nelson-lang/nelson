%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 64-->
if ispc() && ~havecompiler()
  configuremsvc()
end
skip_testsuite(~havecompiler())
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
%=============================================================================
destinationdir = tempname();
mkdir(destinationdir);
cleanupDestination = onCleanup(@() rmdir(destinationdir, 's')); %#ok<NASGU>
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
asserts.istrue(isfile(destinationPath));
[status, message] = dlmake(destinationdir);
asserts.istrue(status);
[R, msg] = unix(['"', destinationdir, '/test_executable_cpp', '"']);
asserts.istrue(R == 0);
asserts.istrue(startsWith(msg, 'It works with C++!!!'));
%=============================================================================
