%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mexEvalStringWithTrap') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mexEvalStringWithTrap.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mexEvalStringWithTrap.c');
  addpath(pwd())
end
%=============================================================================
R = mexEvalStringWithTrap('A');
asserts.isequal(R, 'It works !!!');
%=============================================================================
R = mexEvalStringWithTrap();
asserts.isequal(class(R), 'MException');
REF = 'Undefined variable or function: NOT_EXIST';
asserts.isequal(R.message, REF);
%=============================================================================
