%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
matver = {'-v6', '-v7', '-v7.3'};
%=============================================================================
R = 'Nelson supports character array';
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_char_vector', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = ['Nel'; 'son'];
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_char_array', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = char(zeros(3, 0));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_char_empty', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
