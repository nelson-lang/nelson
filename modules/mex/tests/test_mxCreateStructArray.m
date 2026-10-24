%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
skip_testsuite(~findcmake(), 'C toolchain (cmake) not available')
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
if exist('mxCreateStructArray') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxCreateStructArray.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxCreateStructArray.c');
  addpath(pwd())
end
%=============================================================================
R = mxCreateStructArray();
asserts.istrue(isstruct(R));
asserts.isequal(size(R), [4 1]);
%=============================================================================
asserts.isequal(R(1).fullname, 'Michael B.');
asserts.isequal(R(1).date, 11122016);
%=============================================================================
asserts.isequal(R(2).fullname, 'Pierre P.');
asserts.isequal(R(2).date, 11122017);
%=============================================================================
asserts.isequal(R(3).fullname, 'Nicolas M.');
asserts.isequal(R(3).date, 11122018);
%=============================================================================
asserts.isequal(R(4).fullname, 'Manu T.');
asserts.isequal(R(4).date, 11122019);
%=============================================================================
