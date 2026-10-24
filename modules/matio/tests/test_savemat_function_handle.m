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
f = @(x) a * x + b;
%=============================================================================
for v = matver
  test_file_mat = [tempname(), '.mat'];
  savemat(test_file_mat, 'f', v{1});
  clear f;
  loadmat(test_file_mat);
  asserts.istrue(isstruct(f));
  asserts.isequal(size(f), [1 1]);
  asserts.isequal(f.function_handle, '@(x)a*x+b');
  asserts.isequal(f.a, 1);
  asserts.isequal(f.b, 2);
  rmfile(test_file_mat);
end
%=============================================================================
