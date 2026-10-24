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
if exist('mxGetClassName') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxGetClassName.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxGetClassName.c');
  addpath(pwd())
end
%=============================================================================
R = mxGetClassName(pi);
asserts.isequal(R, 'double');
%=============================================================================
R = mxGetClassName(single(pi));
asserts.isequal(R, 'single');
%=============================================================================
R = mxGetClassName(true);
asserts.isequal(R, 'logical');
%=============================================================================
R = mxGetClassName("string");
asserts.isequal(R, 'string');
%=============================================================================
R = mxGetClassName(int8(3));
asserts.isequal(R, 'int8');
%=============================================================================
S.ce = 1;
S.st = 'r';
R = mxGetClassName(S);
asserts.isequal(R, 'struct');
%=============================================================================
C = {'1', 4};
R = mxGetClassName(C);
asserts.isequal(R, 'cell');
%=============================================================================
f = figure(33);
R = mxGetClassName(f);
asserts.isequal(R, 'nelson.ui.Figure');
%=============================================================================
