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
if exist('mxGetFieldByNumber') == 0
  test_dir = [tempdir(), 'mxGetFieldByNumber'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxGetFieldByNumber.c', test_dir);
  assert_istrue(status);
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
assert_isequal(R, 1);
R = mxGetFieldByNumber(C, 0, 1);
assert_isequal(R, single(2));
R = mxGetFieldByNumber(C, 0, 2);
assert_isequal(R, true);
R = mxGetFieldByNumber(C, 1, 0);
assert_isequal(R, 3);
R = mxGetFieldByNumber(C, 1, 1);
assert_isequal(R, single(4));
R = mxGetFieldByNumber(C, 1, 2);
assert_isequal(R, false);
R = mxGetFieldByNumber(C, 2, 2);
assert_isequal(R, false);
%=============================================================================
