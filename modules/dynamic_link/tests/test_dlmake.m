%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
destinationdir = [tempdir(), 'test_cmake_2'];
mkdir(destinationdir);
%=============================================================================
currentpath = fileparts(nfilename('fullpathext'));
copyfile([currentpath, '/test_build_c.c'], [destinationdir, '/test_build_c.c']);
copyfile([currentpath, '/test_build_c.h'], [destinationdir, '/test_build_c.h']);
%=============================================================================
[status, message] = dlgeneratemake(destinationdir, ...
'test_c', ...
[destinationdir, '/test_build_c.c'], ...
[destinationdir, '/']);
if ~status
  error(message);
end
[status, message] = dlmake(destinationdir);
assert_istrue(status);
%=============================================================================