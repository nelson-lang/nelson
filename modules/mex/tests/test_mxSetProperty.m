%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxSetProperty') == 0
  test_dir = [tempdir(), 'mxSetProperty'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxSetProperty.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxSetProperty.c');
  addpath(pwd())
end
%=============================================================================
F = figure(44);
REF =  F.Color / 4;
R = mxSetProperty(F);
assert_isapprox(R.Color, REF, 1e-4);
%=============================================================================

