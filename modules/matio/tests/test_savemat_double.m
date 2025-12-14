%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
matver = {'-v4', '-v6', '-v7', '-v7.3'};
arch = computer('arch');
if strcmp(arch, 'woa64')
  matver(1) = []; % '-v4' is not supported on Windows on ARM64
end
%=============================================================================
R = eye(3, 4);
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_double', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = eye(0, 4);
R_REF = R;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_double_empty', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isequal(R, R_REF);
end
%=============================================================================
R = rand(3, 4, 5);
R_REF = R;
matver = {'-v7', '-v7.3'};
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_double_rand', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isapprox(R, R_REF, 1e-4);
end
%=============================================================================
R = rand(3, 4) + i;
R_REF_R = real(R);
R_REF_I = imag(R);
matver = {'-v7', '-v7.3'};
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_rand_cplx', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isapprox(real(R), R_REF_R, 1e-4);
  assert_isapprox(imag(R), R_REF_I, 1e-4);
end
%=============================================================================
R = rand(3, 4, 5) + i;
R_REF_R = real(R);
R_REF_I = imag(R);
matver = {'-v7', '-v7.3'};
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_rand_dcplx', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isapprox(real(R), R_REF_R, 1e-4);
  assert_isapprox(imag(R), R_REF_I, 1e-4);
end
%=============================================================================
