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
if exist('mxCreateCharMatrixFromStrings') == 0
  test_dir = [tempdir(), 'mxCreateCharMatrixFromStrings'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxCreateCharMatrixFromStrings.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxCreateCharMatrixFromStrings.c');
  addpath(pwd())
end
%=============================================================================
R = mxCreateCharMatrixFromStrings();
assert_isequal(size(R), [3 21]);
assert_isequal(double(R(1, :)), [118, 97, 108, 117, 101, 0, 0, 0, 0, 0,  0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
assert_isequal(double(R(2, :)), [97, 110, 111, 116, 104, 101, 114, 32, 118, 97, 108, 117, 101, 0, 0, 0, 0, 0, 0, 0, 0]);
assert_isequal(double(R(3, :)), [97, 110, 111, 116, 104, 101, 114, 32, 118, 97, 108, 117, 101, 32, 108, 111, 110, 103, 101, 115, 116]);
%=============================================================================
