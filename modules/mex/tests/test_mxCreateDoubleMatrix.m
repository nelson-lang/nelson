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
if exist('mxCreateDoubleMatrix') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxCreateDoubleMatrix.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxCreateDoubleMatrix.c');
  addpath(pwd())
end
%=============================================================================
R = mxCreateDoubleMatrix(0);
asserts.isequal(R, zeros(2, 3));
%=============================================================================
R = mxCreateDoubleMatrix(1);
asserts.isequal(R, complex(zeros(2, 3)));
%=============================================================================
R = mxCreateDoubleMatrix(2);
asserts.isequal(R, [1 2]);
%=============================================================================
R = mxCreateDoubleMatrix(3);
asserts.isequal(R, [1 + 3i 2 + 4i]);
%=============================================================================