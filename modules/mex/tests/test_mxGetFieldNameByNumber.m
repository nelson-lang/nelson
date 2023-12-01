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
if exist('mxGetFieldNameByNumber') == 0
  test_dir = [tempdir(), 'mxGetFieldNameByNumber'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxGetFieldNameByNumber.c', test_dir);
  assert_istrue(status);
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
assert_isequal(R, 'A');
%=============================================================================
R = mxGetFieldNameByNumber(S, 1);
assert_isequal(R, 'B');
%=============================================================================
R = mxGetFieldNameByNumber(S, 2);
assert_isequal(R, 'C');
%=============================================================================
R = mxGetFieldNameByNumber(S, 3);
assert_isequal(R, 'D');
%=============================================================================
