%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
asserts.istrue(havecompiler());
%=============================================================================
if exist('mxDuplicateArray') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxDuplicateArray.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxDuplicateArray.c');
  addpath(pwd())
end
%=============================================================================
REF = {1, single(2); 'Nelson Text in a cell', false};
R = mxDuplicateArray(REF);
asserts.istrue(iscell(R));
asserts.isequal(R{1}, REF{1});
asserts.isequal(R{2}, REF{2});
asserts.isequal(R{3}, REF{3});
asserts.isequal(R{4}, REF{4});
asserts.isequal(R, REF);
%=============================================================================
