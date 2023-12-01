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
if exist('mexPutVariable') == 0
  test_dir = [tempdir(), 'mexPutVariable'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexPutVariable.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexPutVariable.c');
  addpath(pwd())
end
%=============================================================================
mexPutVariable('base', 'AA', 33);
R = acquirevar('base', 'AA');
assert_isequal(R, 33);
%=============================================================================
mexPutVariable('global', 'BB', 44);
R = acquirevar('global', 'BB');
assert_isequal(R, 44);
%=============================================================================