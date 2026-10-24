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
if ~ispc(); skip_testsuite('Native installer validation requires Windows.'); end
ncc('--help');
__nelsonc_inno_engine__();
try
check_reproducible();

catch installer_error
  skip_installer_antivirus(installer_error);
end
function check_reproducible()
  directory = [tempname(), ' repro ', char([28450, 23383])];
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  sources = {};
  applications = {};
  snapshots = {};
  for k = 1:2
    sources{k} = fullfile(directory, ['source_', int2str(k)]);
    make_source(sources{k}, k);
    snapshots{k} = metadata('snapshot', sources{k}, '');
    applications{k} = compiler.build.standaloneApplication(fullfile(sources{k}, 'repro_entry.m'), ...
      'EmbedArchive', false, 'AdditionalFiles', fullfile(sources{k}, 'embedded.txt'), ...
      'OutputDir', fullfile(directory, ['application_', int2str(k)]));
  end
  installers = {};
  kinds = {'none', 'installer', 'runtime'};
  for index = 1:numel(kinds)
    outputs = {};
    for k = 1:2
      output = fullfile(directory, [kinds{index}, '_', int2str(k)]);
      build_installer(kinds{index}, applications{k}, sources{k}, output, 'zip', k == 2);
      outputs{k} = fullfile(output, 'ReproSetup.zip');
      unzip(outputs{k}, fullfile(output, 'expanded'));
    end
    asserts.isequal(sha256(outputs{1}, '-file'), sha256(outputs{2}, '-file'), kinds{index});
    first = fullfile(fileparts(outputs{1}), 'expanded', 'ReproSetup.exe');
    second = fullfile(fileparts(outputs{2}), 'expanded', 'ReproSetup.exe');
    asserts.isequal(sha256(first, '-file'), sha256(second, '-file'), kinds{index});
    installers{index} = second;
  end
  output = fullfile(directory, 'auto');
  build_installer('none', applications{1}, sources{1}, output, 'auto', false);
  asserts.isequal(sha256(fullfile(output, 'ReproSetup.exe'), '-file'), sha256(installers{1}, '-file'));
  for k = 1:2
    asserts.isequal(metadata('snapshot', sources{k}, ''), snapshots{k});
    rmdir(sources{k}, 's');
  end
  for k = 1:numel(kinds)
    install_and_run(kinds{k}, installers{k}, applications{2}, directory);
  end
  clear cleanup;
end

function make_source(directory, variant)
  mkdir(fullfile(directory, 'assets', 'empty'));
  names = {'alpha.txt', 'zeta.txt'};
  if variant == 2; names = names(end:-1:1); end
  for k = 1:numel(names)
    filewrite(fullfile(directory, 'assets', names{k}), ['resource ', names{k}]);
  end
  filewrite(fullfile(directory, 'embedded.txt'), 'REPRO_SOURCE_FREE');
  filewrite(fullfile(directory, 'repro_entry.m'), ['function repro_entry(); ', ...
    'root = fileparts(mfilename(''fullpath'')); ', ...
    'disp(fileread(fullfile(root, ''embedded.txt''))); end']);
  timestamp = '2001-02-03T04:05:06Z';
  if variant == 2; timestamp = '2032-08-09T10:11:12Z'; end
  metadata('set', directory, timestamp);
end

function build_installer(kind, application, source, output, packageType, useReport)
  input = application;
  if useReport; input = fullfile(application.Options.OutputDir, 'buildresult.json'); end
  if strcmp(kind, 'runtime')
    compiler.runtime.customInstaller('ReproSetup', input, 'RuntimeDelivery', 'installer', ...
      'PackageType', packageType, 'OutputDir', output);
    return;
  end
  options = compiler.package.InstallerOptions(application, 'InstallerName', 'ReproSetup', ...
    'RuntimeDelivery', kind, 'OutputDir', output, 'PackageType', packageType, ...
    'AdditionalFiles', fullfile(source, 'assets'), 'Shortcut', '');
  if useReport
    compiler.package.installer(application.Files, input, 'Options', options);
  else
    compiler.package.installer(input, 'Options', options);
  end
end

function install_and_run(kind, installer, application, directory)
  report = jsondecode(fileread(fullfile(application.Options.OutputDir, 'buildresult.json')));
  target = fullfile(directory, 'installed', kind);
  shared = strcmp(kind, 'runtime');
  if shared; target = fullfile(target, report.engineFingerprint); end
  while numel(target) < 200; target = fullfile(target, 'long_destination'); end
  log = fullfile(directory, [kind, '-install.log']);
  [status, output] = system(['"', installer, '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER /PORTABLE=1 /DIR="', ...
    target, '" /LOG="', log, '"'], 90);
  if status ~= 0 && isfile(log); output = fileread(log); end
  asserts.isequal(status, 0, output);
  if shared
    asserts.istrue(~isfolder(fullfile(target, '.nelson-runtime-update')), fileread(log));
  end
  previous = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previous));
  executable = fullfile(target, 'repro_entry.exe');
  installedFile = fullfile(target, 'assets', 'alpha.txt');
  if shared
    setenv('NELSONC_RUNTIME_ROOT', target);
    executable = application.Files{1};
    index = find(endsWith({report.runtime.files.relative}, '/libnlsEngine.dll'));
    asserts.isequal(numel(index), 1);
    installedFile = fullfile(target, report.runtime.files(index).relative);
  else
    setenv('NELSONC_RUNTIME_ROOT', '');
    if strcmp(kind, 'none'); setenv('NELSONC_RUNTIME_ROOT', nelsonroot()); end
    asserts.isequal(sha256(executable, '-file'), sha256(application.Files{1}, '-file'));
    asserts.isequal(sha256(fullfile(target, 'repro_entry.nca'), '-file'), sha256(application.Files{2}, '-file'));
    asserts.istrue(isfolder(fullfile(target, 'assets', 'empty')));
    asserts.isequal(fileread(installedFile), 'resource alpha.txt');
  end
  if ~strcmp(kind, 'none')
    runtime = target;
    if ~shared; runtime = fullfile(target, 'repro_entry.runtime'); end
    longest = max(cellfun(@numel, {report.runtime.files.relative}));
    asserts.istrue(numel(runtime) + longest > 260);
    for k = 1:numel(report.runtime.files)
      item = report.runtime.files(k);
      asserts.isequal(sha256(fullfile(runtime, item.relative), '-file'), item.sha256);
    end
  end
  metadata('check', installedFile, '1980-01-01T00:00:00Z');
  [status, output] = system(['"', executable, '"'], 60);
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, 'REPRO_SOURCE_FREE'), output);
  clear restore;
  filewrite(fullfile(target, 'user-output.txt'), 'KEEP');
  uninstall = dir(fullfile(target, 'unins*.exe'));
  asserts.isequal(numel(uninstall), 1);
  uninstallLog = fullfile(directory, [kind, '-uninstall.log']);
  [status, output] = system(['"', fullfile(target, uninstall.name), ...
    '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /LOG="', uninstallLog, '"'], 90);
  if status ~= 0 && isfile(uninstallLog); output = fileread(uninstallLog); end
  asserts.isequal(status, 0, output);
  asserts.isfalse(isfile(installedFile));
  asserts.isequal(fileread(fullfile(target, 'user-output.txt')), 'KEEP');
end

function output = metadata(mode, target, timestamp)
  script = fullfile(modulepath('compiler', 'tests'), 'test_installer_timestamps.ps1');
  [status, output] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', script, ...
    '" -Mode ', mode, ' -Target "', target, '" -Timestamp "', timestamp, '"'], 30);
  asserts.isequal(status, 0, output);
end
