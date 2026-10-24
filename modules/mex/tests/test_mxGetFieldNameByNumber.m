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
if exist('mxGetFieldNameByNumber') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxGetFieldNameByNumber.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxGetFieldNameByNumber.c');
  addpath(pwd())
end
%=============================================================================
S.A = 1;
S.B = single(2);
S.C = true;
S.D = 3;
%=============================================================================
R = mxGetFieldNameByNumber(S, 0);
asserts.isequal(R, 'A');
%=============================================================================
R = mxGetFieldNameByNumber(S, 1);
asserts.isequal(R, 'B');
%=============================================================================
R = mxGetFieldNameByNumber(S, 2);
asserts.isequal(R, 'C');
%=============================================================================
R = mxGetFieldNameByNumber(S, 3);
asserts.isequal(R, 'D');
%=============================================================================
