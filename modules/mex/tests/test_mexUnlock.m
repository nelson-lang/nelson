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
if exist('mexUnlock') == 0
  test_dir = [tempdir(), 'mexUnlock'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexUnlock.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexUnlock.c');
  addpath(pwd())
end
%=============================================================================
mexUnlock();
R = evalc('clear mex');
assert_isequal(R, 'Call at Exit');
R = evalc('clear mexUnlock');
assert_isequal(R, '');
R = evalc('clear all');
assert_isequal(R, '');
%=============================================================================
