%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
GIT_REPOSITORY = 'https://github.com/nelson-lang/repo_builtin_tests.git';
LOCAL_DIRECTORY = [tempdir(), 'test_repo_export', '_', createGUID()];
if isdir(LOCAL_DIRECTORY)
  rmdir(LOCAL_DIRECTORY, 's');
end
mkdir(LOCAL_DIRECTORY);
try
  repo('export', GIT_REPOSITORY, LOCAL_DIRECTORY)
catch ex
  skip_testsuite(ex.message)
end

R = dir(LOCAL_DIRECTORY);
assert_isequal(length(R), 5);
REF_NAMES = {'.', '..', 'LICENSE', 'README.md', 'repo_test.nlf'};
for k = 1:length(R)
  assert_isequal(R(k).name, REF_NAMES{k});
end
%=============================================================================
if isdir(LOCAL_DIRECTORY)
  rmdir(LOCAL_DIRECTORY, 's');
end
%=============================================================================
