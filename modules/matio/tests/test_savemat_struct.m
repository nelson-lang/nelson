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
R = struct();
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_struct_empty_1', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = struct([]);
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_struct_empty_2', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = struct();
R.a = 1;
R.b = 'f';
R.c = R;
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_struct', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
