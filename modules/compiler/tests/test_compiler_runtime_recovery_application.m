%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 600-->
% <--SEQUENTIAL TEST REQUIRED-->
ncc('--help');
try
if ispc(); check_application(); end

catch installer_error
  skip_installer_antivirus(installer_error);
end

function check_application()
  oldPath = path();
  restorePath = onCleanup(@() path(oldPath));
  addpath(fullfile(modulepath('compiler'), 'tests'));
  work = tempname(fullfile(nelsonroot(), 'temp'));
  mkdir(work);
  cleanup = onCleanup(@() remove_work(work));
  source = fullfile(work, 'source');
  mkdir(source);
  firstEntry = fullfile(source, 'recovery_a.m');
  filewrite(firstEntry, 'function recovery_a(); if ~isdeployed() || ismodule(''compiler''); error(''Invalid runtime.''); end; disp(''RECOVERY_A_OK''); end');
  first = compiler.build.standaloneApplication(firstEntry, 'OutputDir', fullfile(work, 'app-a'));
  secondEntry = fullfile(source, 'recovery_b.m');
  filewrite(secondEntry, 'function recovery_b(); if sin(0) ~= 0; error(''Invalid result.''); end; disp(''RECOVERY_B_OK''); end');
  second = compiler.build.standaloneWindowsApplication(secondEntry, 'OutputDir', fullfile(work, 'app-b'));
  firstPlan = __nelsonc_distribution_plan__(first);
  secondPlan = __nelsonc_distribution_plan__(second);
  asserts.isfalse(any(strcmp(firstPlan.runtime.modules, 'trigonometric_functions')));
  asserts.istrue(any(strcmp(secondPlan.runtime.modules, 'trigonometric_functions')));
  a = build_runtime(work, 'first', firstPlan);
  b = build_runtime(work, 'second', secondPlan);
  rmdir(source, 's');
  helper = fullfile(modulepath('compiler'), 'tests', 'test_runtime_recovery_process.ps1');
  [status, output] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', helper, '" -Work "', work, ...
    '" -First "', a, '" -Second "', b, '" -AppFirst "', first.Files{1}, ...
    '" -AppSecond "', second.Files{1}, '"'], 250);
  asserts.istrue(status == 0, output);
  asserts.istrue(contains(output, 'RUNTIME_APPLICATION_RECOVERY_OK'), output);
  disp(output);
  clear cleanup;
end

function installer = build_runtime(work, name, plan)
  stage = fullfile(work, name);
  payload = fullfile(stage, 'payload');
  mkdir(stage);
  __nelsonc_copy_runtime__(plan.runtime, payload);
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
