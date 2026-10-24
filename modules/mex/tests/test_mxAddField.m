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
if exist('mxAddField') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxAddField.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxAddField.c');
  addpath(pwd())
end
%=============================================================================
S.f1 = 'A';
S.f2 = 3;
REF = S;
REF.field_added = 44;
R = mxAddField(S);
asserts.isequal(R, REF);
%=============================================================================