%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxAssertS') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxAssertS.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxAssertS.c');
  addpath(pwd())
end
%=============================================================================
R = mxAssertS(1);
%=============================================================================
M = [];
try
  mxAssertS(0);
catch
  M = lasterror();
end
asserts.isequal(M.identifier, 'Nelson:MEX');
asserts.istrue(contains(M.message, 'Assertion failed: at'));
%=============================================================================