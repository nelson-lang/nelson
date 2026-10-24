%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[status, output] = system('git --version');
if status ~= 0
  skip_testsuite(['git executable not available: ', output]);
end
%=============================================================================
root = [tempname(), ' space ', char(233)];
source = [root, '/source'];
cloneDefault = [root, '/clone default'];
cloneBranch = [root, '/clone branch'];
cloneUserPass = [root, '/clone userpass'];
cloneOptions = [root, '/clone options'];
exportDefault = [root, '/export default'];
exportBranch = [root, '/export branch'];
exportUserPass = [root, '/export userpass'];
exportOptions = [root, '/export options'];
if isdir(root)
  rmdir(root, 's');
end
mkdir(root);
mkdir(source);
%=============================================================================
try
  runGit(source, 'init');
  runGit(source, 'config user.email "repo@example.test"');
  runGit(source, 'config user.name "Nelson Repo Test"');
  filewrite([source, '/README.md'], 'initial');
  runGit(source, 'add README.md');
  runGit(source, 'commit -m "Initial commit"');
  runGit(source, 'branch -M master');
  runGit(source, 'checkout -b branch_dev');
  filewrite([source, '/branch.txt'], 'branch');
  runGit(source, 'add branch.txt');
  runGit(source, 'commit -m "Branch commit"');
  runGit(source, 'tag v1.0.0');
  runGit(source, 'checkout master');
catch ex
  if isdir(root)
    rmdir(root, 's');
  end
  skip_testsuite(ex.message);
end
%=============================================================================
repo('clone', source, cloneDefault);
asserts.istrue(isdir([cloneDefault, '/.git']));
asserts.istrue(isfile([cloneDefault, '/README.md']));
asserts.isequal(repo('current_branch', cloneDefault), 'master');
branches = repo('branch', cloneDefault);
asserts.istrue(any(contains(branches, 'master')));
asserts.istrue(any(contains(branches, 'origin/branch_dev')));
tags = repo('tag', cloneDefault);
asserts.istrue(any(contains(tags, 'v1.0.0')));
logs = repo('log', cloneDefault);
asserts.istrue(length(logs) >= 1);
asserts.isequal(logs(end).message, 'Initial commit');
%=============================================================================
repo('checkout', cloneDefault, 'branch_dev');
asserts.isequal(repo('current_branch', cloneDefault), 'branch_dev');
asserts.istrue(isfile([cloneDefault, '/branch.txt']));
repo('checkout', cloneDefault, 'v1.0.0');
currentBranch = repo('current_branch', cloneDefault);
asserts.isequal(currentBranch(1), '(');
asserts.isequal(currentBranch(end), ')');
asserts.istrue(contains(currentBranch, 'HEAD'));
branchLogs = repo('log', cloneDefault);
repo('checkout', cloneDefault, branchLogs(1).sha1);
currentBranch = repo('current_branch', cloneDefault);
asserts.isequal(currentBranch(1), '(');
asserts.isequal(currentBranch(end), ')');
asserts.istrue(contains(currentBranch, 'HEAD'));
repo('checkout', cloneDefault, 'master');
repo('remove_branch', cloneDefault, 'branch_dev');
branches = repo('branch', cloneDefault);
asserts.isfalse(any(strcmp(branches, 'branch_dev')));
asserts.istrue(any(contains(branches, 'origin/branch_dev')));
%=============================================================================
repo('clone', source, 'branch_dev', cloneBranch, '', '');
asserts.isequal(repo('current_branch', cloneBranch), 'branch_dev');
asserts.istrue(isfile([cloneBranch, '/branch.txt']));
repo('clone', source, cloneUserPass, '', '');
asserts.isequal(repo('current_branch', cloneUserPass), 'master');
repo('clone', source, cloneOptions, 'UseAgent', false, 'PrivateKey', '', 'PublicKey', '', 'Passphrase', '');
asserts.isequal(repo('current_branch', cloneOptions), 'master');
repo('fetch', cloneOptions, 'UseAgent', false);
%=============================================================================
repo('export', source, exportDefault);
asserts.istrue(isfile([exportDefault, '/README.md']));
asserts.isfalse(isdir([exportDefault, '/.git']));
repo('export', source, exportUserPass, '', '');
asserts.istrue(isfile([exportUserPass, '/README.md']));
asserts.isfalse(isdir([exportUserPass, '/.git']));
repo('export', source, 'branch_dev', exportBranch, '', '');
asserts.istrue(isfile([exportBranch, '/branch.txt']));
asserts.isfalse(isdir([exportBranch, '/.git']));
repo('export', source, exportOptions, 'UseAgent', false, 'PrivateKey', '', 'PublicKey', '', 'Passphrase', '');
asserts.istrue(isfile([exportOptions, '/README.md']));
asserts.isfalse(isdir([exportOptions, '/.git']));
%=============================================================================
if isdir(root)
  rmdir(root, 's');
end
%=============================================================================
function runGit(directory, arguments)
  command = buildGitCommand(directory, arguments);
  [status, output] = system(command);
  if status ~= 0
    error(['git command failed: ', output]);
  end
end
%=============================================================================
function command = buildGitCommand(directory, arguments)
  if ispc()
    command = ['cd /d ', quotePath(directory), ' && git ', arguments];
  else
    command = ['cd ', quotePath(directory), ' && git ', arguments];
  end
end
%=============================================================================
function quoted = quotePath(path)
  quoted = ['"', strrep(path, '"', '\"'), '"'];
end
%=============================================================================
