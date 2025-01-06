%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
assert_istrue(havecompiler());
%=============================================================================
if exist('mxDuplicateArray') == 0
  test_dir = [tempdir(), 'mxDuplicateArray_cell'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxDuplicateArray.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxDuplicateArray.c');
  addpath(pwd())
end
%=============================================================================
REF = {1, single(2); 'Nelson Text in a cell', false};
R = mxDuplicateArray(REF);
assert_istrue(iscell(R));
assert_isequal(R{1}, REF{1});
assert_isequal(R{2}, REF{2});
assert_isequal(R{3}, REF{3});
assert_isequal(R{4}, REF{4});
assert_isequal(R, REF);
%=============================================================================
