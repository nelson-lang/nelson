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
a = 1;
b = 2;
f = @(x) x + 1;
%=============================================================================
for v = matver
  test_file_mat = [tempdir(), 'test_save_function_handle', v{1}, '.mat'];
  savemat(test_file_mat, 'f', v{1});
  clear f;
  loadmat(test_file_mat);
  assert_istrue(isstruct(f));
  assert_isequal(size(f), [1 1]);
  assert_isequal(f.function_handle, '@(x) x+1');
  assert_isequal(f.a, 1);
  assert_isequal(f.b, 2);
end
%=============================================================================
