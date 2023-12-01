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
if exist('mexAtExit') == 0
  test_dir = [tempdir(), 'mexAtExit'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexAtExit.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexAtExit.c');
  addpath(pwd())
end
%=============================================================================
mexAtExit();
R = evalc('clear mex');
assert_isequal(R, 'Call at Exit');
%=============================================================================
R = evalc('clear mexAtExit');
assert_isequal(R, '');
%=============================================================================
mexAtExit();
R = evalc('clear mexAtExit');
assert_isequal(R, 'Call at Exit');
%=============================================================================
mexAtExit();
R = evalc('clear all');
assert_isequal(R, 'Call at Exit');
%=============================================================================
