%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mexUnlock') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mexUnlock.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mexUnlock.c');
  addpath(pwd())
end
%=============================================================================
mexUnlock();
R = evalc('clear mex');
asserts.isequal(R, 'Call at Exit');
R = evalc('clear mexUnlock');
asserts.isequal(R, '');
R = evalc('clear all');
asserts.isequal(R, '');
%=============================================================================
