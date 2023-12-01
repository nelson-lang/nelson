%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('repo'), 2);
assert_isequal(nargout('repo'), 1);
%=============================================================================
GIT_REPOSITORY = 'https://github.com/nelson-lang/repo_builtin_tests.git';
LOCAL_DIRECTORY = [tempdir(), 'test_repo', '_', createGUID()];
if isdir(LOCAL_DIRECTORY)
  rmdir(LOCAL_DIRECTORY, 's');
end
mkdir(LOCAL_DIRECTORY);
repo('clone', GIT_REPOSITORY, LOCAL_DIRECTORY)
R = dir(LOCAL_DIRECTORY);
assert_isequal(length(R), 6);
REF_NAMES = {'.', '..', '.git', 'LICENSE', 'README.md', 'repo_test.nlf'};
for k = 1:length(R)
  assert_isequal(R(k).name, REF_NAMES{k});
end
%=============================================================================
TAGS = repo('tag', LOCAL_DIRECTORY);
REF_TAGS = {'v0.0.1'; 'v0.0.2'};
assert_isequal(TAGS, REF_TAGS);
%=============================================================================
BRANCHES = repo('branch', LOCAL_DIRECTORY);
REF_BRANCHES = {'master'; 'origin/branch_dev'; 'origin/master'};
assert_istrue(any(contains(BRANCHES, REF_BRANCHES)));
%=============================================================================
repo('checkout', LOCAL_DIRECTORY, 'branch_dev');
BRANCHES = repo('branch', LOCAL_DIRECTORY);
REF_BRANCHES = {'branch_dev'; 'master'; 'origin/branch_dev'; 'origin/master'};
assert_istrue(any(contains(BRANCHES, REF_BRANCHES)));
%=============================================================================
st = repo('log', LOCAL_DIRECTORY);
assert_isequal(length(st), 16);
assert_isequal(st(end).sha1, '1a8564c8');
assert_isequal(st(end).message, 'Initial commit');
assert_isequal(st(end).author, 'Allan CORNET <nelson.numerical.computation@gmail.com>');
assert_istrue(startsWith(st(end).date, 'Mon Oct 21 '));
assert_istrue(endsWith(st(end).date, ':02:25 2019'));
%=============================================================================
assert_isequal(repo('current_branch', LOCAL_DIRECTORY), 'branch_dev')
%=============================================================================
repo('checkout', LOCAL_DIRECTORY, 'master');
assert_isequal(repo('current_branch', LOCAL_DIRECTORY), 'master');
repo('remove_branch', LOCAL_DIRECTORY, 'branch_dev');
BRANCHES = repo('branch', LOCAL_DIRECTORY);
REF_BRANCHES = {'master'; 'origin/branch_dev'; 'origin/master'};
assert_istrue(any(contains(BRANCHES, REF_BRANCHES)));
%=============================================================================
% fetch can require an authentification
% repo('fetch', LOCAL_DIRECTORY);
repo('checkout', LOCAL_DIRECTORY, 'branch_dev');
BRANCHES = repo('branch', LOCAL_DIRECTORY);
REF_BRANCHES = {'branch_dev'; 'master'; 'origin/branch_dev'; 'origin/master'};
assert_istrue(any(contains(BRANCHES, REF_BRANCHES)));
%=============================================================================
if isdir(LOCAL_DIRECTORY)
  rmdir(LOCAL_DIRECTORY, 's');
end
%=============================================================================
GIT_REPOSITORY = 'https://github.com/nelson-lang/repo_builtin_tests.git';
LOCAL_DIRECTORY = [tempdir(), 'test_repo', '_', createGUID()];
if isdir(LOCAL_DIRECTORY)
  rmdir(LOCAL_DIRECTORY, 's');
end
mkdir(LOCAL_DIRECTORY);
repo('clone', GIT_REPOSITORY, LOCAL_DIRECTORY, "", "")
R = dir(LOCAL_DIRECTORY);
assert_isequal(length(R), 6);
REF_NAMES = {'.', '..', '.git', 'LICENSE', 'README.md', 'repo_test.nlf'};
for k = 1:length(R)
  assert_isequal(R(k).name, REF_NAMES{k});
end
%=============================================================================
if isdir(LOCAL_DIRECTORY)
  rmdir(LOCAL_DIRECTORY, 's');
end
%=============================================================================
