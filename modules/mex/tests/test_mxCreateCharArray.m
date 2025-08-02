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
if exist('mxCreateCharArray') == 0
  test_dir = [tempdir(), 'mxCreateCharArray'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxCreateCharArray.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxCreateCharArray.c');
  addpath(pwd())
end
%=============================================================================
R = mxCreateCharArray();
REF = ['ac'; 'bd'];
assert_isequal(R, REF);
%=============================================================================
