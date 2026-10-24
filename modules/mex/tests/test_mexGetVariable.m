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
if exist('mexGetVariable') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mexGetVariable.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mexGetVariable.c');
  addpath(pwd())
end
%=============================================================================
assignin('global', 'BB', 44);
asserts.isfalse(isvar('BB'));
R = mexGetVariable('global', 'BB');
asserts.isequal(R, 44);
%=============================================================================
assignin('base', 'AA', 33);
R = mexGetVariable('base', 'AA');
AA = acquirevar('base', 'AA');
asserts.isequal(R, AA);
%=============================================================================
