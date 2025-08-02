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
R = single(eye(3, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_single', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = single(eye(0, 4));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_single_empty', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = single(rand(3, 4, 5));
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_single_rand', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isapprox(R, R_REF, 1e-4);
end
%=============================================================================
R = single(rand(3, 4, 5) + i);
R_REF_R = real(R);
R_REF_I = imag(R);
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_single_rand', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isapprox(real(R), R_REF_R, 1e-4);
  assert_isapprox(imag(R), R_REF_I, 1e-4);
end
%=============================================================================
