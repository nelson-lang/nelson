%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxAssertS') == 0
  test_dir = [tempdir(), 'mxAssertS'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxAssertS.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxAssertS.c');
  addpath(pwd())
end
%=============================================================================
R = mxAssertS(1);
%=============================================================================
M = [];
try
  mxAssertS(0);
catch
  M = lasterror();
end
assert_isequal(M.identifier, 'Nelson:MEX');
assert_istrue(contains(M.message, 'Assertion failed: at'));
%=============================================================================