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
R = logical(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_logical', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = logical(eye(0, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_logical_empty', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = logical(rand(3, 4, 5));
R_REF = R;
matver = {'-v7', '-v7.3'};
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_logical_rand', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
