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
if exist('mxIsInteger') == 0
  test_dir = [tempdir(), 'mxIsInteger'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxIsInteger.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxIsInteger.c');
  addpath(pwd())
end
%=============================================================================
assert_isequal(mxIsInteger(int8(3)), 'int8');
assert_isequal(mxIsInteger(int16(3)), 'int16');
assert_isequal(mxIsInteger(int32(3)), 'int32');
assert_isequal(mxIsInteger(int64(3)), 'int64');
%=============================================================================
assert_isequal(mxIsInteger(uint8(3)), 'uint8');
assert_isequal(mxIsInteger(uint16(3)), 'uint16');
assert_isequal(mxIsInteger(uint32(3)), 'uint32');
assert_isequal(mxIsInteger(uint64(3)), 'uint64');
%=============================================================================
assert_isequal(mxIsInteger(true), 'not integer');
%=============================================================================
