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
R = int8(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_eye_', class(R), v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = int16(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_eye_', class(R), v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = int32(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_eye_', class(R), v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = int64(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_eye_', class(R), v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = uint8(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_eye_', class(R), v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = uint16(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_eye_', class(R), v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = uint32(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_eye_', class(R), v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = uint64(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_eye_', class(R), v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
