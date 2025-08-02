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
if exist('mxCreateStructArray') == 0
  test_dir = [tempdir(), 'mxCreateStructArray'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxCreateStructArray.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxCreateStructArray.c');
  addpath(pwd())
end
%=============================================================================
R = mxCreateStructArray();
assert_istrue(isstruct(R));
assert_isequal(size(R), [4 1]);
%=============================================================================
assert_isequal(R(1).fullname, 'Michael B.');
assert_isequal(R(1).date, 11122016);
%=============================================================================
assert_isequal(R(2).fullname, 'Pierre P.');
assert_isequal(R(2).date, 11122017);
%=============================================================================
assert_isequal(R(3).fullname, 'Nicolas M.');
assert_isequal(R(3).date, 11122018);
%=============================================================================
assert_isequal(R(4).fullname, 'Manu T.');
assert_isequal(R(4).date, 11122019);
%=============================================================================
