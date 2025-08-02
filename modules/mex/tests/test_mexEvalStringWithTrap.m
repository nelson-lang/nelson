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
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mexEvalStringWithTrap') == 0
  test_dir = [tempdir(), 'mexEvalStringWithTrap'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexEvalStringWithTrap.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexEvalStringWithTrap.c');
  addpath(pwd())
end
%=============================================================================
R = mexEvalStringWithTrap('A');
assert_isequal(R, 'It works !!!');
%=============================================================================
R = mexEvalStringWithTrap();
assert_isequal(class(R), 'MException');
REF = 'Undefined variable or function: NOT_EXIST';
assert_isequal(R.message, REF);
%=============================================================================
