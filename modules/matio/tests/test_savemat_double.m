%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
matver = {'-v4', '-v6', '-v7', '-v7.3'};
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
  test_file_mat = [tempdir(), 'test_save_rand', v{1}, '.mat'];
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
  test_file_mat = [tempdir(), 'test_save_rand', v{1}, '.mat'];
  savemat(test_file_mat, 'R', v{1});
  clear R;
  loadmat(test_file_mat);
  assert_isapprox(real(R), R_REF_R, 1e-4);
  assert_isapprox(imag(R), R_REF_I, 1e-4);
end
%=============================================================================
