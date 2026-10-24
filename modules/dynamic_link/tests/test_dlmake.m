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
destinationdir = [tempdir(), 'test_cmake_2_', createGUID()];
mkdir(destinationdir);
cleanupDestination = onCleanup(@() rmdir(destinationdir, 's'));
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
asserts.istrue(status);
asserts.istrue(isfile([destinationdir, '/test_c', getdynlibext()]));
if ~ispc()
  asserts.isfalse(isfile([destinationdir, '/libtest_c', getdynlibext()]));
end
%=============================================================================
