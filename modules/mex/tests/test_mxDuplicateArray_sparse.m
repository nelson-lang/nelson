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
i = [800 1100 20];
j = [900 1000 30];
v = [10 100 20];
S = sparse(i, j, v, 1300, 1500, 30);
R = mxDuplicateArray(S);
asserts.isequal(R, S);
asserts.isequal(nzmax(R), nzmax(S));
%=============================================================================
i = [800 1100 20];
j = [900 1000 30];
v = [true true true];
S = sparse(i, j, v, 1300, 1500, 30);
R = mxDuplicateArray(S);
asserts.isequal(R, S);
asserts.isequal(nzmax(R), nzmax(S));
%=============================================================================
% separated complex representation
i = [800 1100 20];
j = [900 1000 30];
v = complex([10 100 20], 1);
S = sparse(i, j, v, 1300, 1500, 30);
R = mxDuplicateArray(S);
asserts.isequal(R, S);
asserts.isequal(nzmax(R), nzmax(S));
%=============================================================================
