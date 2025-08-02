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
if exist('mxDuplicateArrayInterleavedComplex') == 0
  test_dir = [tempdir(), 'mxDuplicateArray_interleaved'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxDuplicateArray.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('-output', 'mxDuplicateArrayInterleavedComplex', '-R2018a', 'mxDuplicateArray.c' );
  addpath(pwd())
end
%=============================================================================
% interleaved complex representation
i = [800 1100 20];
j = [900 1000 30];
v = complex([10 100 20], 1);
S = sparse(i, j, v, 1300, 1500, 30);
R = mxDuplicateArrayInterleavedComplex(S);
assert_isequal(R, S);
assert_isequal(nzmax(R), nzmax(S));
%=============================================================================
