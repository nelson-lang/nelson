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
if exist('mxSetClassName') == 0
  test_dir = [tempdir(), 'mxSetClassName'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxSetClassName.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxSetClassName.c');
  addpath(pwd())
end
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
ST.r = 1;
ST.i = 3;
R = mxSetClassName(ST);
assert_isequal(class(R), 'complexObj');
%=============================================================================
