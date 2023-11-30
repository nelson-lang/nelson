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
if exist('mexCallMATLAB') == 0
  test_dir = [tempdir(), 'mexCallMATLAB'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mexCallMATLAB.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mexCallMATLAB.c');
  addpath(pwd())
end
%=============================================================================
R = mexCallMATLAB(pi);
assert_isequal(R, sin(pi));
%=============================================================================
assert_checkerror('R = mexCallMATLAB(''a'');', 'error');
%=============================================================================
