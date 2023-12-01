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
  configuremsvc();
end
%=============================================================================
if exist('mxCreateDoubleMatrix') == 0
  test_dir = [tempdir(), 'mxCreateDoubleMatrix'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxCreateDoubleMatrix.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxCreateDoubleMatrix.c');
  addpath(pwd())
end
%=============================================================================
R = mxCreateDoubleMatrix(0);
assert_isequal(R, zeros(2, 3));
%=============================================================================
R = mxCreateDoubleMatrix(1);
assert_isequal(R, complex(zeros(2, 3)));
%=============================================================================
R = mxCreateDoubleMatrix(2);
assert_isequal(R, [1 2]);
%=============================================================================
R = mxCreateDoubleMatrix(3);
assert_isequal(R, [1+3i 2+4i]);
%=============================================================================