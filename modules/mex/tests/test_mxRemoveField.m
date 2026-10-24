%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxRemoveField') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxRemoveField.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxRemoveField.c');
  addpath(pwd())
end
%=============================================================================
S.f1 = 1;
S.f2 = 2;
S.f3 = 3;
R = mxRemoveField(S);
REF.f1 = 1;
REF.f3 = 3;
asserts.isequal(R, REF);
%=============================================================================
