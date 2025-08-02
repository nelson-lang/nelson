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
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mexCallMATLABWithTrap') == 0
  test_dir = [tempdir(), 'mexCallMATLABWithTrap'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexCallMATLABWithTrap.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexCallMATLABWithTrap.c');
  addpath(pwd())
end
%=============================================================================
R = mexCallMATLABWithTrap(pi);
assert_isequal(R, 'It works !!!');
%=============================================================================
R = mexCallMATLABWithTrap('A');
assert_isequal(class(R), 'MException');
%=============================================================================
REF = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'sin');
assert_isequal(R.message, REF);
%=============================================================================
