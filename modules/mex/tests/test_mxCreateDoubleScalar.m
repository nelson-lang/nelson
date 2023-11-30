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
if exist('mxCreateDoubleScalar') == 0
  test_dir = [tempdir(), 'mxCreateDoubleScalar'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxCreateDoubleScalar.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxCreateDoubleScalar.c');
  addpath(pwd())
end
%=============================================================================
R = mxCreateDoubleScalar();
assert_isequal(R, 33);
%=============================================================================