%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
plan = struct('searchRoots', {{'source-root'}}, 'files', []);
record = struct('root', 'source-root', 'relative', 'nested/empty');
plan.directories = record;
plan.pathEntries = [record, record];
[roots, paths] = __nelsonc_stage_paths__(plan, fullfile(directory, 'valid'));
asserts.isequal(roots, {'roots/1'});
asserts.isequal(paths, {'roots/1/nested/empty', 'roots/1'});
asserts.istrue(isfolder(fullfile(directory, 'valid/roots/1/nested/empty')));
invalid = {'../outside', '/absolute', './local', 'nested/../escape', 'nested//empty', ...
  'nested/', 'C:/absolute', 'nested\escape', ['nul', char(0)], 1, {'nested'}};
for k = 1:numel(invalid)
  bad = plan;
  bad.directories.relative = invalid{k};
  assert_invalid(bad, fullfile(directory, ['invalid-', num2str(k)]));
end
bad = plan;
bad.pathEntries(1).root = 'unknown-root';
assert_invalid(bad, fullfile(directory, 'unknown'));
bad = plan;
bad.pathEntries = 'not-records';
assert_invalid(bad, fullfile(directory, 'type'));
bad = plan;
bad.files = struct('module', '', 'root', 'source-root', 'relative', '../escape');
assert_invalid(bad, fullfile(directory, 'file'));
clear cleanup;

function assert_invalid(plan, destination)
  identifier = '';
  try
    __nelsonc_stage_paths__(plan, destination);
  catch ex
    identifier = ex.identifier;
  end
  asserts.istrue(strcmp(identifier, 'Nelson:nelsonc:inputPath') || startsWith(identifier, 'Nelson:nelsonc:inputPath:'));
  asserts.isfalse(isfolder(destination));
end
