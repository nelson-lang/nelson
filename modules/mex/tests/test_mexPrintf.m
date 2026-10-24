%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mexPrintf') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mexPrintf.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mexPrintf.c');
  addpath(pwd())
end
%=============================================================================
R = evalc('mexPrintf()');
REF = 'Result 0
Result 1
';
asserts.isequal(R, REF);
%=============================================================================