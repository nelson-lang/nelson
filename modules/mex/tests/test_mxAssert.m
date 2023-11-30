%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxAssert') == 0
  test_dir = [tempdir(), 'mxAssert'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxAssert.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxAssert.c');
  addpath(pwd())
end
%=============================================================================
R = mxAssert(1);
%=============================================================================
M = [];
try
  mxAssert(0);
catch
  M = lasterror();
end
assert_isequal(M.identifier, 'Nelson:MEX');
assert_istrue(contains(M.message, 'Assertion failed: (int)expr'));
%=============================================================================