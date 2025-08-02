%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxDuplicateArray') == 0
  test_dir = [tempdir(), 'mxDuplicateArray_gh'];
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
F = figure(33);
R = mxDuplicateArray(F);
assert_isequal(R.Number, F.Number);
%=============================================================================
