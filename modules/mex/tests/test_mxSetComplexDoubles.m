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
if exist('mxSetComplexDoubles') == 0
  test_dir = [tempdir(), 'mxSetComplexDoubles'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxSetComplexDoubles.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxSetComplexDoubles.c', '-R2018a');
  addpath(pwd())
end
%=============================================================================
R = mxSetComplexDoubles();
REF = [complex(3, 9); complex(4, 8)];
assert_isequal(R, REF);
%=============================================================================
