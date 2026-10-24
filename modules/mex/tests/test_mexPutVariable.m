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
if exist('mexPutVariable') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mexPutVariable.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mexPutVariable.c');
  addpath(pwd())
end
%=============================================================================
mexPutVariable('base', 'AA', 33);
R = acquirevar('base', 'AA');
asserts.isequal(R, 33);
%=============================================================================
mexPutVariable('global', 'BB', 44);
R = acquirevar('global', 'BB');
asserts.isequal(R, 44);
%=============================================================================