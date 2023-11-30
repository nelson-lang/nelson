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
if exist('mxRemoveField') == 0
  test_dir = [tempdir(), 'mxRemoveField'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxRemoveField.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxRemoveField.c');
  addpath(pwd())
end
%=============================================================================
S.f1 = 1;
S.f2 = 2;
S.f3 = 3;
R = mxRemoveField(S);
REF.f1 = 1;
REF.f3 = 3;
assert_isequal(R, REF);
%=============================================================================
