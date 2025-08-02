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
if exist('mexLock') == 0
  test_dir = [tempdir(), 'mexLock'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexLock.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexLock.c');
  addpath(pwd())
end
%=============================================================================
mexLock();
R = evalc('clear mex');
assert_isequal(R, '');
R = evalc('clear mexLock');
assert_isequal(R, '');
R = evalc('clear all');
assert_isequal(R, '');
%=============================================================================
