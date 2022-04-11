%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
matver = {'-v7', '-v7.3'};
%=============================================================================
f = str2func('cos');
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_function_handle', v{1}, '.mat'];
  savemat(test_file_mat, 'f', v{1});
  clear f;
  loadmat(test_file_mat);
  assert_istrue(isstruct(f));
  assert_isequal(fieldnames(f), {'name'; 'anonymous'});
  assert_isequal(size(f), [1 1]);
  assert_isequal(f.name, 'cos');
  assert_isequal(f.anonymous, '');
end
%=============================================================================
