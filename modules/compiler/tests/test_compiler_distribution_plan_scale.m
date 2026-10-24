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
source = fullfile(directory, 'synthetic.bin');
filewrite(source, 'Synthetic distribution file identity.');
digest = sha256(source, '-file');
platform = 'linux';
if ispc()
  platform = 'x64';
  if strcmp(computer('arch'), 'woa64')
    platform = 'ARM64';
  end
elseif ismac()
  platform = 'macOS';
end
binary = ['bin/', platform];
count = 5000;
names = cell(1, count + 2);
names{1} = [binary, '/libnlsInterpreter', getdynlibext()];
names{2} = [binary, '/libnlsEngine', getdynlibext()];
for k = 1:count
  names{k + 2} = sprintf('resources/bulk_%05d.dat', k);
end
% Synthetic identities exercise merge size without loading an application.
files = struct('relative', names, 'sha256', repmat({digest}, 1, count + 2), ...
  'source', repmat({source}, 1, count + 2));
runtime = struct('format', 1, 'modules', {{'core', 'engine', 'interpreter'}}, ...
  'moduleOrder', {{'core', 'engine', 'interpreter'}}, ...
  'startupOrder', {{'core', 'engine', 'interpreter'}}, ...
  'files', files, 'inputs', struct('source', source, 'sha256', digest), ...
  'directories', {{'bin', binary, 'modules', 'resources'}}, ...
  'libraries', struct([]), 'bootstrapFiles', {{}}, 'systemLibraries', {{}}, ...
  'gui', false, 'dynamicStartup', false, 'mode', 'installed');
launcher = struct('format', 1, 'startupInterface', 1, 'archiveMinVersion', 1, ...
  'archiveMaxVersion', 2, 'runtimeDiscoveryVersion', 2, 'kind', 'console');
first = struct('format', 2, 'kind', 'nelson-standalone-build', 'architecture', computer('arch'), ...
  'runtimeVersion', '1.0', 'engineFingerprint', digest, 'applicationName', 'first', ...
  'executableVersion', '1.0.0.0', 'buildType', 'standaloneApplication', 'executable', 'app', ...
  'files', struct('relative', 'app', 'sha256', digest), 'includedSupportPackages', {{}}, ...
  'runtime', runtime, 'launcher', launcher);
second = first;
second.applicationName = 'second';
first.runtime.files = files(1:(count / 2 + 2));
second.runtime.files = files([1, 2, (count / 2 + 3):end]);
encoded = jsonencode({first, second});
started = tic();
plan = __nelsonc_plan__('distribution', encoded, true);
elapsed = toc(started);
asserts.isequal(plan, __nelsonc_plan__('distribution', jsonencode({second, first}), true));
document = jsondecode(plan);
asserts.isequal(numel(document.runtime.files), count + 2);
asserts.isequal(document.applications, {'first'; 'second'});
asserts.istrue(any(strcmp({document.runtime.files.relative}, 'resources/bulk_00001.dat')));
asserts.istrue(any(strcmp({document.runtime.files.relative}, 'resources/bulk_05000.dat')));
fprintf('DISTRIBUTION_MERGE files=%d elapsed=%.6f\n', count + 2, elapsed);
clear cleanup;
