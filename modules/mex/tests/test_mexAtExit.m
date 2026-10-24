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
if exist('mexAtExit') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mexAtExit.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mexAtExit.c');
  addpath(pwd())
end
%=============================================================================
mexAtExit();
R = evalc('clear mex');
asserts.isequal(R, 'Call at Exit');
%=============================================================================
R = evalc('clear mexAtExit');
asserts.isequal(R, '');
%=============================================================================
mexAtExit();
R = evalc('clear mexAtExit');
asserts.isequal(R, 'Call at Exit');
%=============================================================================
mexAtExit();
R = evalc('clear all');
asserts.isequal(R, 'Call at Exit');
%=============================================================================
