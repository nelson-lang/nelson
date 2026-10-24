%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
source = fullfile(directory, 'source');
target = fullfile(directory, 'target');
mkdir(source);
mkdir(target);
filewrite(fullfile(source, 'application.bin'), char([0, 1, 65, 127]));
filewrite(fullfile(source, 'readme.txt'), 'new readme');
filewrite(fullfile(target, 'readme.txt'), 'existing readme');
assert_rejected(@() __nelsonc_executable__('publish', source, target));
asserts.isfalse(isfile(fullfile(target, 'application.bin')));
asserts.isequal(fileread(fullfile(target, 'readme.txt')), 'existing readme');
delete(fullfile(target, 'readme.txt'));
__nelsonc_executable__('publish', source, target);
for name = {'application.bin', 'readme.txt'}
  asserts.isequal(sha256(fullfile(source, name{1}), '-file'), ...
    sha256(fullfile(target, name{1}), '-file'));
end
assert_rejected(@() __nelsonc_executable__('publish', source, target));
assert_rejected(@() __nelsonc_executable__('publish', source, source));
assert_rejected(@() __nelsonc_executable__('publish', [source, char(0)], target));
empty = fullfile(directory, 'empty');
mkdir(empty);
assert_rejected(@() __nelsonc_executable__('publish', empty, source));
mkdir(fullfile(empty, 'subdirectory'));
assert_rejected(@() __nelsonc_executable__('publish', empty, source));
clear cleanup;

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
