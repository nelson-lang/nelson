%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
if exist('mxGetClassName') == 0
  test_dir = [tempdir(), 'mxGetClassName'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxGetClassName.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxGetClassName.c');
  addpath(pwd())
end
%=============================================================================
R = mxGetClassName(pi);
assert_isequal(R, 'double');
%=============================================================================
R = mxGetClassName(single(pi));
assert_isequal(R, 'single');
%=============================================================================
R = mxGetClassName(true);
assert_isequal(R, 'logical');
%=============================================================================
R = mxGetClassName("string");
assert_isequal(R, 'string');
%=============================================================================
R = mxGetClassName(int8(3));
assert_isequal(R, 'int8');
%=============================================================================
S.ce = 1;
S.st = 'r';
R = mxGetClassName(S);
assert_isequal(R, 'struct');
%=============================================================================
C = {'1', 4};
R = mxGetClassName(C);
assert_isequal(R, 'cell');
%=============================================================================
f = figure(33);
R = mxGetClassName(f);
assert_isequal(R, 'graphics_object');
%=============================================================================
