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
if exist('mxGetFieldByNumber') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxGetFieldByNumber.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxGetFieldByNumber.c');
  addpath(pwd())
end
%=============================================================================
C(1).A = 1;
C(1).B = single(2);
C(1).C = true;
C(2).A = 3;
C(2).B = single(4);
C(2).C = false;
R = mxGetFieldByNumber(C, 0, 0);
asserts.isequal(R, 1);
R = mxGetFieldByNumber(C, 0, 1);
asserts.isequal(R, single(2));
R = mxGetFieldByNumber(C, 0, 2);
asserts.isequal(R, true);
R = mxGetFieldByNumber(C, 1, 0);
asserts.isequal(R, 3);
R = mxGetFieldByNumber(C, 1, 1);
asserts.isequal(R, single(4));
R = mxGetFieldByNumber(C, 1, 2);
asserts.isequal(R, false);
R = mxGetFieldByNumber(C, 2, 2);
asserts.isequal(R, false);
%=============================================================================
