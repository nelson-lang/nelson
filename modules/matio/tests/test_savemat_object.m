%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
matver = {'-v7', '-v7.3'};
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
cplx = complexObj(3, 4);
REF = struct();
REF.r = 3;
REF.i = 4;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_function_object', v{1}, '.mat'];
  savemat(test_file_mat, 'cplx', v{1});
  clear cplx;
  loadmat(test_file_mat);
  assert_isequal(cplx.r, REF.r);
  assert_isequal(cplx.i, REF.i);
end
%=============================================================================
