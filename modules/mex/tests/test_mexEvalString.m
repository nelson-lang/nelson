%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mexEvalString') == 0
  test_dir = [tempdir(), 'mexEvalString'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexEvalString.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexEvalString.c');
  addpath(pwd())
end
%=============================================================================
mexEvalString();
assert_isequal(A, 'Hello World');
assert_isequal(B, 100);
%=============================================================================
