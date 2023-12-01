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
if exist('mxCreateSparse') == 0
  test_dir = [tempdir(), 'mxCreateSparse'];
  if isdir(test_dir)
    rmdir(test_dir,'s');
  end
  mkdir(test_dir);
  status = copyfile('mxCreateSparse.c', test_dir);
  assert_istrue(status);
  cd(test_dir);
  mex('mxCreateSparse.c');
  addpath(pwd())
end
%=============================================================================
REF = sparse(eye(10, 10));
S = full(REF);
R = mxCreateSparse(S);
assert_isequal(R, REF);
%=============================================================================
REF = sparse(eye(5, 3));
S = full(REF);
R = mxCreateSparse(S);
assert_isequal(R, REF);
%=============================================================================
REF = sparse(eye(5, 3) + eye(5, 3) * i);
S = full(REF);
R = mxCreateSparse(S);
assert_isequal(R, REF);
%=============================================================================
REF = sparse(logical(eye(5, 3)));
S = full(REF);
R = mxCreateSparse(S);
assert_isequal(R, REF);
%=============================================================================
