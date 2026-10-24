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
if exist('mexCallMATLAB') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mexCallMATLAB.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mexCallMATLAB.c');
  addpath(pwd())
end
%=============================================================================
R = mexCallMATLAB(pi);
asserts.isequal(R, sin(pi));
%=============================================================================
asserts.checkerror('R = mexCallMATLAB(''a'');', 'error');
%=============================================================================
