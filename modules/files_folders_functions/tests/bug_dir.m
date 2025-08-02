%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
__dir1  = [tempdir(), 'bug_dir_1'];
mkdir(__dir1);
for k = 1:10
  file_name_to_create = [tempdir(), 'bug_dir_1/file_', int2str(k)];
  if ~isfile(file_name_to_create)
    fp = fopen(file_name_to_create, 'wt');
    fclose(fp);
  end
end

A = dir([tempdir(), 'bug_dir_1']);
B = dir([tempdir(), 'bug_dir_1/']);
assert_isequal(A, B);
assert_isequal(size(A), [12 1]);
%=============================================================================
