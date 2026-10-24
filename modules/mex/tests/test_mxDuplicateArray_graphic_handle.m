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
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxDuplicateArray') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxDuplicateArray.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxDuplicateArray.c');
  addpath(pwd())
end
%=============================================================================
F = figure(33);
R = mxDuplicateArray(F);
asserts.isequal(R.Number, F.Number);
%=============================================================================
