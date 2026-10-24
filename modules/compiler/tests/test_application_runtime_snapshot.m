%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
source = fullfile(directory, 'source');
binaries = fullfile(source, 'bin', 'test-platform');
module = fullfile(source, 'modules', 'snapshot_module');
mkdir(binaries);
mkdir(fullfile(module, 'functions'));
mkdir(fullfile(module, 'tests'));
for name = {'node_modules', 'temp', 'test-results'}
  mkdir(fullfile(module, name{1}));
  write_text(fullfile(module, name{1}, 'excluded.txt'), 'Development data must not be copied.');
end
mkdir(fullfile(module, 'dist', 'node_modules'));
write_text(fullfile(module, 'dist', 'node_modules', 'runtime.js'), 'Nested runtime resource.');
mkdir(fullfile(source, 'etc'));
mkdir(fullfile(source, 'resources', 'empty'));
for name = {'gpl-3.0.md', 'lgpl-3.0.md', 'THIRDPARTY.md', 'TRADEMARKS.md'}
  write_text(fullfile(source, name{1}), 'Synthetic snapshot test notice.');
end
order = fullfile(source, 'modules', 'modules.m');
write_text(order, 'modules_list = {{''unselected'', false}; {''snapshot_module'', true}};');
bootstrap = fullfile(module, 'loader.m');
write_text(bootstrap, '% Bootstrap must not be executed while copying.');
macro = fullfile(module, 'functions', 'snapshot_macro.m');
write_text(macro, 'function value = snapshot_macro(); value = 1; end');
write_text(fullfile(module, 'tests', 'excluded.m'), 'This test directory must not be copied.');
resource = fullfile(source, 'resources', 'data.txt');
write_text(resource, 'original resource');
library = fullfile(binaries, 'snapshot.dll');
write_text(library, 'Synthetic native-library snapshot fixture.');
runtime = struct('format', 1, 'modules', {{'snapshot_module'}}, 'gui', false);
runtime.libraries = struct('source', library, 'filename', 'snapshot.dll', 'sha256', sha256(library, '-file'));
runtime.inputs = struct('source', bootstrap, 'sha256', sha256(bootstrap, '-file'));
runtime.bootstrapFiles = {bootstrap};
snapshot = __nelsonc_runtime_files__(runtime, source, binaries);
if ispc()
  platform = 'x64';
  if strcmp(computer('arch'), 'woa64')
    platform = 'ARM64';
  end
elseif ismac()
  platform = 'macOS';
else
  platform = 'linux';
end
asserts.istrue(any(strcmp({snapshot.files.relative}, ['bin/', platform, '/snapshot.dll'])));
asserts.isfalse(any(contains(string({snapshot.files.relative}), '/test-platform/')));
asserts.isequal(snapshot.moduleOrder, {'snapshot_module'});
asserts.isfalse(any(contains(string({snapshot.files.relative}), '/tests/')));
for name = {'node_modules', 'temp', 'test-results'}
  prefix = ['modules/snapshot_module/', name{1}, '/'];
  asserts.isfalse(any(startsWith(string({snapshot.files.relative}), prefix)));
end
asserts.istrue(any(strcmp({snapshot.files.relative}, 'modules/snapshot_module/dist/node_modules/runtime.js')));
asserts.istrue(any(strcmp(snapshot.directories, 'resources/empty')));
asserts.isequal(jsonencode(snapshot), jsonencode(__nelsonc_runtime_files__(runtime, source, binaries)));

write_text(fullfile(module, 'functions', 'added_after_analysis.m'), 'This file is outside the frozen inventory.');
destination = fullfile(directory, 'copy');
__nelsonc_copy_runtime__(snapshot, destination);
asserts.isfalse(isfile(fullfile(destination, 'modules', 'snapshot_module', 'functions', 'added_after_analysis.m')));
asserts.istrue(isfolder(fullfile(destination, 'resources', 'empty')));
inventory = jsondecode(fileread(fullfile(destination, 'runtime.json')));
asserts.isfalse(isfield(inventory.files, 'source'));
asserts.isfalse(isfield(inventory.libraries, 'source'));
asserts.isfalse(isfield(inventory, 'inputs'));
for k = 1:numel(inventory.files)
  item = inventory.files(k);
  asserts.isequal(sha256(fullfile(destination, item.relative), '-file'), item.sha256);
end
asserts.isequal(sha256(fullfile(destination, inventory.generated.relative), '-file'), inventory.generated.sha256);
asserts.istrue(contains(fileread(fullfile(destination, 'modules', 'modules.m')), 'snapshot_module'));
asserts.isfalse(contains(fileread(fullfile(destination, 'modules', 'modules.m')), 'unselected'));

write_text(resource, 'changed resource');
expect_failure(snapshot, fullfile(directory, 'changed-resource'), 'Nelson:nelsonc:runtimeChanged:runtimeFileChangedAfterAnalysis');
write_text(resource, 'original resource');
write_text(library, 'Changed native library.');
expect_failure(snapshot, fullfile(directory, 'changed-library'), 'Nelson:nelsonc:runtimeChanged:runtimeFileChangedAfterAnalysis');
write_text(library, 'Synthetic native-library snapshot fixture.');
write_text(bootstrap, '% Changed bootstrap.');
expect_failure(snapshot, fullfile(directory, 'changed-bootstrap'), 'Nelson:nelsonc:runtimeChanged');
asserts.isfalse(isfolder(fullfile(directory, 'changed-bootstrap')));
write_text(bootstrap, '% Bootstrap must not be executed while copying.');
write_text(order, 'error(''The changed module order must never execute during copying.'');');
expect_failure(snapshot, fullfile(directory, 'changed-order'), 'Nelson:nelsonc:runtimeChanged');
write_text(order, 'modules_list = {{''unselected'', false}; {''snapshot_module'', true}};');

invalid = snapshot;
invalid.files(1).relative = '../escaped.txt';
expect_failure(invalid, fullfile(directory, 'invalid-path'), 'Nelson:nelsonc:runtimePath');
asserts.isfalse(isfile(fullfile(directory, 'escaped.txt')));
invalid = snapshot;
invalid.files(1).relative = 'modules/modules.m';
expect_failure(invalid, fullfile(directory, 'invalid-reserved'), 'Nelson:nelsonc:runtimeCollision:conflictingRuntimeDestinationPaths');
expect_failure(snapshot, destination, 'Nelson:nelsonc:runtimeDestination');

large = runtime;
count = 7000;
names = cell(1, count);
for k = 1:count
  names{k} = sprintf('snapshot_%05d.dll', count - k + 1);
end
large.libraries = struct('source', repmat({library}, 1, count), ...
  'filename', names, 'sha256', repmat({sha256(library, '-file')}, 1, count));
large.libraries(end + 1) = large.libraries(1);
inventory = __nelsonc_runtime_files__(large, source, binaries);
native = inventory.files(startsWith(string({inventory.files.relative}), 'bin/'));
asserts.isequal(numel(native), count);
asserts.istrue(endsWith(native(1).relative, 'snapshot_00001.dll'));
asserts.istrue(endsWith(native(end).relative, 'snapshot_07000.dll'));
large.libraries(end).sha256 = repmat('0', 1, 64);
failed = false;
try
  __nelsonc_runtime_files__(large, source, binaries);
catch ex
  failed = true;
  asserts.isequal(ex.identifier, 'Nelson:nelsonc:runtimeCollision:runtimeInputsShareDestination');
end
asserts.istrue(failed);
clear cleanup;

function expect_failure(snapshot, destination, identifier)
  failed = false;
  try
    __nelsonc_copy_runtime__(snapshot, destination);
  catch ex
    failed = true;
    asserts.isequal(ex.identifier, identifier);
  end
  asserts.istrue(failed);
end

function write_text(filename, text)
  fid = fopen(filename, 'wt');
  asserts.istrue(fid >= 0);
  fprintf(fid, '%s', text);
  fclose(fid);
end
