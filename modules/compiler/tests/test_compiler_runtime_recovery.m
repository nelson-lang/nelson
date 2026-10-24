%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 300-->
% <--SEQUENTIAL TEST REQUIRED-->
ncc('--help');
try
if ispc(); check_runtime_recovery(); end

catch installer_error
  skip_installer_antivirus(installer_error);
end

function check_runtime_recovery()
  oldPath = path();
  restorePath = onCleanup(@() path(oldPath));
  addpath(fullfile(modulepath('compiler'), 'tests'));
  work = tempname(fullfile(nelsonroot(), 'temp'));
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  first = build_fixture(work, 'first', false);
  second = build_fixture(work, 'second', true);
  helper = fullfile(modulepath('compiler'), 'tests', 'test_runtime_recovery_process.ps1');
  [status, output] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', helper, '" -Work "', work, ...
    '" -First "', first, '" -Second "', second, '"'], 250);
  asserts.istrue(status == 0, output);
  asserts.istrue(contains(output, 'RUNTIME_PROCESS_RECOVERY_OK'), output);
  disp(output);
  clear cleanup;
end

function installer = build_fixture(work, name, extra)
  stage = fullfile(work, name);
  payload = fullfile(stage, 'payload');
  mkdir(fullfile(payload, 'data'));
  filewrite(fullfile(payload, 'data', 'value-a.txt'), 'first');
  files = struct('relative', 'data/value-a.txt', 'sha256', sha256('first'));
  modules = {'core', 'engine', 'interpreter'};
  if extra
    filewrite(fullfile(payload, 'data', 'value-b.txt'), 'second');
    files(2) = struct('relative', 'data/value-b.txt', 'sha256', sha256('second'));
    modules{end + 1} = 'extra';
  end
  runtime = struct('files', files, 'directories', {{'data', 'modules'}}, ...
    'moduleOrder', {modules}, 'startupOrder', {{'core', 'engine', 'interpreter', 'extra'}}, ...
    'gui', false, 'dynamicStartup', false);
  plan = struct('architecture', computer('arch'), 'runtimeVersion', '2.0.0', ...
    'engineFingerprint', sha256(work), 'runtime', runtime);
  __nelsonc_runtime_inventory__(plan, fullfile(stage, 'runtime-package.ini'));
  options = struct('InstallerName', name, 'PackageType', 'auto', 'Compression', 'normal', 'Verbose', false, 'OutputDir', fullfile(stage, 'output'));
  generator = @(output) runtime_recovery_script(stage, payload, output, plan, options);
  __nelsonc_inno_build__(__nelsonc_inno_engine__(), stage, payload, '', options, generator);
  installer = fullfile(options.OutputDir, [name, '.exe']);
end

function remove_work(work)
  allowed = fullpath(fullfile(nelsonroot(), 'temp'));
  asserts.istrue(startsWith(fullpath(work), [allowed, '/']));
  if isfolder(work); rmdir(work, 's'); end
end
