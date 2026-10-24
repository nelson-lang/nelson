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
% <--TIMEOUT 600-->
if ispc() || ismac(); return; end
ncc('--help');
check_installer();

function check_installer()
  work = [tempname(), ' shared runtime '' ', char([28450, 23383])]; mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  source = fullfile(work, 'source'); mkdir(source);
  firstFile = fullfile(source, 'runtime_a.m');
  filewrite(firstFile, 'function runtime_a(); if ~isdeployed() || ismodule(''compiler''); error(''Invalid runtime.''); end; disp(''RUNTIME_A_OK''); end');
  secondFile = fullfile(source, 'runtime_b.m');
  filewrite(secondFile, 'function runtime_b(); if sin(0) ~= 0; error(''Invalid computation.''); end; disp(''RUNTIME_B_OK''); end');
  first = compiler.build.standaloneApplication(firstFile, 'OutputDir', fullfile(work, 'first'));
  second = compiler.build.standaloneApplication(secondFile, 'OutputDir', fullfile(work, 'second'));
  expected = __nelsonc_distribution_plan__([first, second]);
  output = fullfile(work, 'installers');
  compiler.runtime.customInstaller('first', first, 'RuntimeDelivery', 'installer', 'OutputDir', output);
  report = fullfile(second.Options.OutputDir, 'buildresult.json');
  compiler.runtime.customInstaller('second', report, 'RuntimeDelivery', 'installer', 'OutputDir', output);
  compiler.runtime.customInstaller('first', first, 'RuntimeDelivery', 'installer', 'OutputDir', fullfile(work, 'repeat'));
  asserts.isequal(sha256(fullfile(output, 'first.install'), '-file'), ...
    sha256(fullfile(work, 'repeat', 'first.install'), '-file'));
  legacy = jsondecode(fileread(report)); legacy.launcher.runtimeDiscoveryVersion = 2;
  legacyFile = fullfile(work, 'legacy.json'); filewrite(legacyFile, jsonencode(legacy));
  rejected = false;
  try
    compiler.runtime.customInstaller('old', legacyFile, 'RuntimeDelivery', 'installer', 'OutputDir', output);
  catch ex
    rejected = strcmp(ex.identifier, 'Nelson:nelsonc:launcher');
  end
  asserts.istrue(rejected);
  applications = fullfile(work, 'applications'); mkdir(applications);
  copyfile(first.Files{1}, applications); copyfile(second.Files{1}, applications);
  rmdir(source, 's'); rmdir(first.Options.OutputDir, 's'); rmdir(second.Options.OutputDir, 's');
  variables = {'NELSONC_RUNTIME_ROOT', 'NELSON_RUNTIME_PATH', 'LD_LIBRARY_PATH', 'PATH', ...
    'HOME', 'XDG_CONFIG_HOME', 'XDG_CONFIG_DIRS', 'XDG_DATA_HOME'};
  previous = cellfun(@getenv, variables, 'UniformOutput', false);
  restore = onCleanup(@() restore_environment(variables, previous));
  config = fullfile(work, 'configuration');
  setenv('NELSONC_RUNTIME_ROOT', ''); setenv('NELSON_RUNTIME_PATH', ''); setenv('LD_LIBRARY_PATH', '');
  setenv('PATH', '/usr/bin:/bin'); setenv('HOME', work); setenv('XDG_CONFIG_HOME', config);
  setenv('XDG_CONFIG_DIRS', fullfile(work, 'empty-system')); setenv('XDG_DATA_HOME', fullfile(work, 'data'));
  destination = fullfile(work, 'installed');
  installerA = fullfile(output, 'first.install'); installerB = fullfile(output, 'second.install');
  invoke({installerA, '-destinationFolder', destination}, false);
  invoke({installerA, '-agreeToLicense', 'yes', '-applicationFolder', destination}, false);
  invoke({installerA, '-agreeToLicense', 'yes', '-destinationFolder', destination}, true);
  run_app(fullfile(applications, 'runtime_a'), 'RUNTIME_A_OK');
  asserts.isfalse(isfile(fullfile(destination, 'runtime_a')));
  manifest = jsondecode(fileread(fullfile(destination, 'runtime.json')));
  asserts.isfalse(any(strcmp(manifest.moduleOrder, 'trigonometric_functions')));
  interpreter = fullfile(destination, 'bin', 'linux', 'libnlsInterpreter.so');
  before = dir(interpreter);
  control = fullfile(work, 'control.txt');
  filewrite(control, [char(65279), 'agreeToLicense=yes', char([13, 10]), ...
    'destinationFolder=', destination, char([13, 10]), 'outputFile=', fullfile(work, 'install.log'), char([13, 10])]);
  invoke({installerB, '-inputfile', control}, true);
  after = dir(interpreter); asserts.isequal(after.datenum, before.datenum);
  manifest = jsondecode(fileread(fullfile(destination, 'runtime.json')));
  asserts.isequal(manifest.moduleOrder, expected.runtime.moduleOrder);
  asserts.isequal(numel(manifest.files), numel(expected.runtime.files));
  run_app(fullfile(applications, 'runtime_a'), 'RUNTIME_A_OK');
  run_app(fullfile(applications, 'runtime_b'), 'RUNTIME_B_OK');
  invoke({installerA, '-agreeToLicense', 'yes', '-destinationFolder', destination}, true);
  run_app(fullfile(applications, 'runtime_b'), 'RUNTIME_B_OK');
  rmdir(output, 's');
  filewrite(fullfile(destination, 'user.txt'), 'preserved');
  invoke({fullfile(destination, '.nelson-runtime', 'uninstall')}, true);
  receipt = jsondecode(fileread(fullfile(destination, '.nelson-runtime', 'remove.json')));
  asserts.isequal(receipt.phase, 'complete');
  asserts.isfalse(isfile(fullfile(destination, '.nelson-runtime', 'uninstall')));
  asserts.istrue(isfile(fullfile(destination, 'user.txt')));
  asserts.isfalse(isfile(interpreter));
  asserts.isfalse(isfile(fullfile(config, 'nelson', 'runtimes', 'glnxa64', [expected.engineFingerprint, '.root'])));
  asserts.istrue(isfile(fullfile(applications, 'runtime_a')));
  clear restore cleanup;
end

function output = invoke(commandParts, success)
  quote = char(39);
  for k = 1:numel(commandParts)
    commandParts{k} = [quote, strrep(commandParts{k}, quote, char([39 34 39 34 39])), quote];
  end
  [status, output] = system([strjoin(commandParts, ' '), ' 2>&1'], 150);
  asserts.isequal(status == 0, success, output);
end

function run_app(application, marker)
  output = invoke({application}, true);
  asserts.istrue(contains(output, marker), output);
end

function restore_environment(names, values)
  for k = 1:numel(names); setenv(names{k}, values{k}); end
end
