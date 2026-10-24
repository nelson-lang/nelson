%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
% Installing a runtime package takes the shared runtime lock; only one such
% test (test_compiler_installer_offline) may run inside the parallel block.
ncc('--help');
if ~ispc()
  return;
end
try
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
first = build_fixture(directory, 'first', false);
second = build_fixture(directory, 'second', true);
destination = fullfile(directory, 'runtime');
mkdir(destination);
filewrite(fullfile(destination, 'user.txt'), 'preserved');
[status, output] = install_fixture(first, destination);
asserts.istrue(status ~= 0 && contains(output, 'not empty or managed'), output);
asserts.isequal(fileread(fullfile(destination, 'user.txt')), 'preserved');
destination = fullfile(directory, 'shared runtime', repmat('r', 1, 64));
[status, output] = install_fixture(first, destination);
asserts.istrue(status == 0, output);
manifestFile = fullfile(destination, 'runtime.json');
startupFile = fullfile(destination, 'modules', 'modules.m');
before = sha256(manifestFile, '-file');
startup = fileread(startupFile);
filewrite(startupFile, 'modified');
[status, output] = install_fixture(second, destination);
asserts.istrue(status ~= 0 && contains(output, 'metadata changed'), output);
asserts.isequal(sha256(manifestFile, '-file'), before);
filewrite(startupFile, startup);
[status, output] = install_fixture(second, destination);
asserts.istrue(status == 0, output);
inventory = jsondecode(fileread(manifestFile));
asserts.isequal(numel(inventory.files), 2);
asserts.istrue(any(strcmp(inventory.modules, 'extra')));
for k = 1:numel(inventory.files)
  item = inventory.files(k);
  asserts.isequal(sha256(fullfile(destination, item.relative), '-file'), item.sha256);
end
[status, output] = install_fixture(first, destination);
asserts.istrue(status == 0, output);
asserts.isequal(jsondecode(fileread(manifestFile)), inventory);
uninstall = dir(fullfile(destination, 'unins*.exe'));
asserts.isequal(numel(uninstall), 1);
[status, output] = system(['"', fullfile(destination, uninstall.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90);
asserts.istrue(status == 0, output);
asserts.isfalse(isfile(manifestFile));
clear cleanup;

catch installer_error
  skip_installer_antivirus(installer_error);
end
function installer = build_fixture(directory, name, extra)
  stage = fullfile(directory, name);
  payload = fullfile(stage, 'payload');
  mkdir(payload);
  mkdir(fullfile(payload, 'data'));
  files = struct('relative', 'data/value.txt', 'sha256', sha256('fixture'));
  filewrite(fullfile(payload, files.relative), 'fixture');
  modules = {'core', 'engine', 'interpreter'};
  if extra
    relative = ['data/coefficient_', char(233), '.txt'];
    filewrite(fullfile(payload, relative), 'second');
    files(2) = struct('relative', relative, 'sha256', sha256('second'));
    modules{end + 1} = 'extra';
  end
  runtime = struct('files', files, 'directories', {{'data', 'modules'}}, ...
    'moduleOrder', {modules}, 'startupOrder', {{'core', 'engine', 'interpreter', 'extra'}}, ...
    'gui', false, 'dynamicStartup', false);
  plan = struct('architecture', computer('arch'), 'runtimeVersion', '2.0.0', ...
    'engineFingerprint', sha256(directory), 'runtime', runtime);
  __nelsonc_runtime_inventory__(plan, fullfile(stage, 'runtime-package.ini'));
  options = struct('InstallerName', name, 'PackageType', 'auto', 'Compression', 'normal', 'Verbose', false, 'OutputDir', fullfile(stage, 'output'));
  generator = @(output) __nelsonc_runtime_script__(stage, payload, output, plan, options);
  __nelsonc_inno_build__(__nelsonc_inno_engine__(), stage, payload, '', options, generator);
  installer = fullfile(options.OutputDir, [name, '.exe']);
end

function [status, output] = install_fixture(installer, destination)
  log = [tempname(), '.log'];
  [status, output] = system(['"', installer, '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER /PORTABLE=1 /DIR="', ...
    destination, '" /LOG="', log, '"'], 90);
  if status ~= 0 && isfile(log)
    output = fileread(log);
  end
end
