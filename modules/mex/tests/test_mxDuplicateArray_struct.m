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
if exist('mxDuplicateArray') == 0
  test_dir = [tempdir(), 'mxDuplicateArray_struct'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxDuplicateArray.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxDuplicateArray.c');
  addpath(pwd())
end
%=============================================================================
ST.a = 1;
ST.b = single(2);
ST.c = '读写汉字 - 学中文';
ST.d = false;
R = mxDuplicateArray(ST);
assert_isequal(R, ST);
%=============================================================================
