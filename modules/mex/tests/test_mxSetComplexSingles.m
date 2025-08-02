%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxSetComplexSingles') == 0
  test_dir = [tempdir(), 'mxSetComplexSingles'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxSetComplexSingles.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxSetComplexSingles.c', '-R2018a');
  addpath(pwd())
end
%=============================================================================
R = mxSetComplexSingles();
REF = single([complex(3, 9); complex(4, 8)]);
assert_isequal(R, REF);
%=============================================================================
