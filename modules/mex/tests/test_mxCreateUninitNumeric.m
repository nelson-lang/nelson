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
if exist('mxCreateUninitNumeric') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxCreateUninitNumeric.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxCreateUninitNumeric.c', '-R2018a');
  addpath(pwd())
end
%=============================================================================
R = mxCreateUninitNumeric();
REF = [1.5 2.5 3.5 4.5 6 21 75];
asserts.isequal(R, REF);
%=============================================================================
