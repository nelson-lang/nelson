%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mexGetVariable') == 0
  test_dir = [tempdir(), 'mexGetVariable'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexGetVariable.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexGetVariable.c');
  addpath(pwd())
end
%=============================================================================
assignin('global', 'BB', 44);
assert_isfalse(isvar('BB'));
R = mexGetVariable('global', 'BB');
assert_isequal(R, 44);
%=============================================================================
assignin('base', 'AA', 33);
R = mexGetVariable('base', 'AA');
AA = acquirevar('base', 'AA');
assert_isequal(R, AA);
%=============================================================================
