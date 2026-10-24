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
if exist('mxIsInteger') == 0
  test_dir = tempname();
  mkdir(test_dir);
  status = copyfile('mxIsInteger.c', test_dir);
  asserts.istrue(status);
  cd(test_dir);
  mex('mxIsInteger.c');
  addpath(pwd())
end
%=============================================================================
asserts.isequal(mxIsInteger(int8(3)), 'int8');
asserts.isequal(mxIsInteger(int16(3)), 'int16');
asserts.isequal(mxIsInteger(int32(3)), 'int32');
asserts.isequal(mxIsInteger(int64(3)), 'int64');
%=============================================================================
asserts.isequal(mxIsInteger(uint8(3)), 'uint8');
asserts.isequal(mxIsInteger(uint16(3)), 'uint16');
asserts.isequal(mxIsInteger(uint32(3)), 'uint32');
asserts.isequal(mxIsInteger(uint64(3)), 'uint64');
%=============================================================================
asserts.isequal(mxIsInteger(true), 'not integer');
%=============================================================================
