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
% <--TIMEOUT 900-->
if ispc() || ismac(); return; end
% deployed applications scan PATH (/usr/bin:/bin here) for an installed runtime
skip_testsuite(isfile('/usr/bin/nelson-cli') || isfile('/bin/nelson-cli'), ...
  'A system-wide Nelson installation on PATH is discoverable by deployed applications.');
ncc('--help');
check_installer();

function check_installer()
  work = [tempname(), ' application runtime '' ', char([28450, 23383])]; mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  source = fullfile(work, 'source'); mkdir(source);
  entry = fullfile(source, 'shared_app_a.m');
  filewrite(entry, ['function shared_app_a(output); ', ...
    'if ~isdeployed() || ismodule(''compiler''); error(''Invalid runtime.''); end; ', ...
    'filewrite(output, nelsonroot()); [status, maps] = system(''cat /proc/$PPID/maps''); ', ...
    'if status ~= 0; error(''Cannot inspect loaded libraries.''); end; ', ...
    'filewrite([output, ''.maps''], maps); disp(''SHARED_APP_A_OK''); end']);
  first = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(work, 'first'), 'EmbedArchive', false);
  entryB = fullfile(source, 'shared_app_b.m');
  filewrite(entryB, 'function shared_app_b(); if sin(0) ~= 0; error(''Invalid computation.''); end; disp(''SHARED_APP_B_OK''); end');
  second = compiler.build.standaloneApplication(entryB, 'OutputDir', fullfile(work, 'second'));
  plan = __nelsonc_distribution_plan__([first, second]);
  output = fullfile(work, 'packages');
  firstOptions = compiler.package.InstallerOptions(first, 'RuntimeDelivery', 'installer', ...
    'InstallerName', 'first', 'OutputDir', output, 'DefaultInstallationDir', fullfile(work, 'default'));
  compiler.package.installer(first, 'Options', firstOptions);
  compiler.package.installer(second, 'RuntimeDelivery', 'installer', 'InstallerName', 'second', 'OutputDir', output);
  firstOptions.OutputDir = fullfile(work, 'repeat');
  invoke({'/usr/bin/touch', '-t', '200102030405', first.Files{1}}, true);
  compiler.package.installer(first, 'Options', firstOptions);
  installerA = fullfile(output, 'first.install'); installerB = fullfile(output, 'second.install');
  asserts.isequal(sha256(installerA, '-file'), sha256(fullfile(firstOptions.OutputDir, 'first.install'), '-file'));
  disp('RUNTIME_FOLDER_PACKAGES_READY');
  rmdir(source, 's'); rmdir(first.Options.OutputDir, 's'); rmdir(second.Options.OutputDir, 's');
  variables = {'NELSONC_RUNTIME_ROOT', 'NELSON_RUNTIME_PATH', 'LD_LIBRARY_PATH', 'PATH', ...
    'HOME', 'XDG_CONFIG_HOME', 'XDG_CONFIG_DIRS', 'XDG_DATA_HOME'};
  previous = cellfun(@getenv, variables, 'UniformOutput', false);
  restore = onCleanup(@() restore_environment(variables, previous));
  config = fullfile(work, 'configuration');
  setenv('NELSONC_RUNTIME_ROOT', ''); setenv('NELSON_RUNTIME_PATH', ''); setenv('LD_LIBRARY_PATH', '');
  setenv('PATH', '/usr/bin:/bin'); setenv('HOME', work); setenv('XDG_CONFIG_HOME', config);
  setenv('XDG_CONFIG_DIRS', fullfile(work, 'empty-system')); setenv('XDG_DATA_HOME', fullfile(work, 'data'));
  runtimeParent = fullfile(work, 'shared runtimes');
  runtime = fullfile(runtimeParent, plan.engineFingerprint);
  check_rejections(installerA, work, runtimeParent, plan.engineFingerprint);
  check_publication_failure(installerA, work, plan.engineFingerprint);
  disp('RUNTIME_FOLDER_BOUNDARIES_OK');
  destinationA = fullfile(work, 'default'); destinationB = fullfile(work, 'application B');
  args = {installerA, '-agreeToLicense', 'yes', '-runtimeFolder', [runtimeParent, '/']};
  invoke(args, true);
  asserts.isfalse(isfolder(fullfile(destinationA, 'shared_app_a.runtime')));
  asserts.isfalse(isfolder(fullfile(destinationA, '.nelson-install', 'shared')));
  rootFile = fullfile(work, 'runtime-root.txt');
  applicationA = fullfile(destinationA, 'shared_app_a'); applicationB = fullfile(destinationB, 'shared_app_b');
  result = invoke({applicationA, rootFile}, true);
  asserts.istrue(contains(result, 'SHARED_APP_A_OK'), result);
  asserts.isequal(fileread(rootFile), runtime);
  maps = fileread([rootFile, '.maps']);
  asserts.istrue(contains(maps, [runtime, '/bin/linux/libnlsEngine']));
  asserts.isfalse(contains(maps, modulepath('nelson', 'builtin')));
  permissions = invoke({'/usr/bin/stat', '-c', '%a', fullfile(destinationA, '.nelson-install', 'manifest.tsv')}, true);
  asserts.isequal(strtrim(permissions), '600');
  registration = fullfile(config, 'nelson', 'runtimes', 'glnxa64', [plan.engineFingerprint, '.root']);
  asserts.isequal(fileread(registration), ['NELSON_RUNTIME_ROOT_1', newline(), runtime, newline()]);
  manifest = jsondecode(fileread(fullfile(runtime, 'runtime.json')));
  asserts.isfalse(any(strcmp(manifest.moduleOrder, 'trigonometric_functions')));
  interpreter = fullfile(runtime, 'bin', 'linux', 'libnlsInterpreter.so');
  before = dir(interpreter); digest = sha256(interpreter, '-file');
  appBefore = dir(applicationA);
  control = fullfile(work, 'install.txt');
  filewrite(control, [char(65279), 'agreeToLicense=yes', char([13 10]), ...
    'applicationFolder=', destinationB, char([13 10]), 'runtimeFolder=', runtimeParent, char([13 10])]);
  invoke({installerB, '-inputFile', control}, true);
  asserts.isfalse(isfolder(fullfile(destinationB, 'shared_app_b.runtime')));
  after = dir(interpreter); asserts.isequal(after.datenum, before.datenum);
  asserts.isequal(sha256(interpreter, '-file'), digest);
  manifest = jsondecode(fileread(fullfile(runtime, 'runtime.json')));
  asserts.isequal(manifest.moduleOrder, plan.runtime.moduleOrder);
  asserts.isequal(numel(manifest.files), numel(plan.runtime.files));
  disp('RUNTIME_FOLDER_UNION_OK');
  result = invoke({applicationB}, true); asserts.istrue(contains(result, 'SHARED_APP_B_OK'), result);
  invoke(args, true);
  appAfter = dir(applicationA); asserts.isequal(appAfter.datenum, appBefore.datenum);
  invoke({installerA, '-agreeToLicense', 'yes', '-applicationFolder', destinationA}, false);
  invoke({installerA, '-agreeToLicense', 'yes', '-applicationFolder', destinationA, ...
    '-runtimeFolder', fullfile(work, 'other runtime')}, false);
  asserts.isfalse(isfolder(fullfile(work, 'other runtime')));
  invoke({applicationA, rootFile}, true); invoke({applicationB}, true);
  invoke({fullfile(destinationA, '.nelson-install', 'uninstall.sh')}, true);
  asserts.isfalse(isfolder(destinationA));
  asserts.isequal(sha256(interpreter, '-file'), digest);
  invoke({applicationB}, true);
  invoke({fullfile(runtime, '.nelson-runtime', 'uninstall')}, true);
  asserts.istrue(isfile(applicationB)); asserts.isfalse(isfile(registration));
  invoke({applicationB}, false);
  invoke({installerB, '-inputFile', control}, true);
  invoke({applicationB}, true);
  invoke({fullfile(destinationB, '.nelson-install', 'uninstall.sh')}, true);
  asserts.isfalse(isfolder(destinationB)); asserts.istrue(isfile(interpreter));
  invoke({fullfile(runtime, '.nelson-runtime', 'uninstall')}, true);
  clear restore cleanup;
end

function check_rejections(installer, work, runtimeParent, engine)
  destination = fullfile(work, 'refused');
  prefix = {installer, '-agreeToLicense', 'yes'};
  for args = {{'-destinationFolder', destination, '-runtimeFolder', runtimeParent}, ...
    {'-runtimeFolder', runtimeParent, '-destinationFolder', destination}, ...
    {'-applicationFolder', destination, '-runtimeFolder', 'relative'}, ...
    {'-applicationFolder', destination, '-runtimeFolder', runtimeParent, '-runtimeFolder', runtimeParent}, ...
    {'-applicationFolder', destination, '-runtimeFolder', fullfile(destination, 'runtime')}, ...
    {'-applicationFolder', fullfile(runtimeParent, engine), '-runtimeFolder', runtimeParent}, ...
    {'-applicationFolder', fullfile(runtimeParent, engine, 'app'), '-runtimeFolder', runtimeParent}, ...
    {'-applicationFolder', destination, '-runtimeFolder', [runtimeParent, '/../outside']}}
    invoke([prefix, args{1}], false);
  end
  control = fullfile(work, 'invalid.txt');
  filewrite(control, ['agreeToLicense=yes', newline(), 'runtimeFolder=', runtimeParent, newline()]);
  invoke([prefix, {'-destinationFolder', destination, '-inputFile', control}], false);
  filewrite(control, ['agreeToLicense=yes', newline(), 'destinationFolder=', destination, newline()]);
  invoke([prefix, {'-runtimeFolder', runtimeParent, '-inputFile', control}], false);
  outside = fullfile(work, 'outside'); mkdir(outside);
  filewrite(fullfile(outside, 'keep.txt'), 'KEEP');
  alias = fullfile(work, 'alias');
  invoke({'/usr/bin/ln', '-s', outside, alias}, true);
  invoke([prefix, {'-applicationFolder', destination, '-runtimeFolder', alias}], false);
  asserts.isequal(fileread(fullfile(outside, 'keep.txt')), 'KEEP');
  asserts.isfalse(isfolder(fullfile(outside, engine)));
  asserts.isfalse(isfolder(destination)); asserts.isfalse(isfolder(runtimeParent));
  private = fullfile(work, 'private');
  invoke([prefix, {'-applicationFolder', private}], true);
  invoke([prefix, {'-applicationFolder', private, '-runtimeFolder', runtimeParent}], false);
  asserts.isfalse(isfolder(runtimeParent));
  invoke({fullfile(private, '.nelson-install', 'uninstall.sh')}, true);
  asserts.isfalse(isfolder(private));
end

function check_publication_failure(installer, work, engine)
  destination = fullfile(work, 'publication failure');
  parent = fullfile(work, 'retained runtime');
  runtime = fullfile(parent, engine);
  old = getenv('XDG_CONFIG_HOME');
  restore = onCleanup(@() setenv('XDG_CONFIG_HOME', old));
  % Registration creates the destination after its initial absence check.
  setenv('XDG_CONFIG_HOME', fullfile(destination, 'configuration'));
  output = invoke({installer, '-agreeToLicense', 'yes', '-applicationFolder', destination, ...
    '-runtimeFolder', parent}, false);
  asserts.istrue(contains(output, 'The installation folder was created by another process.'), output);
  asserts.istrue(isfile(fullfile(runtime, 'runtime.json')));
  registration = fullfile(destination, 'configuration', 'nelson', 'runtimes', 'glnxa64', [engine, '.root']);
  asserts.istrue(isfile(registration));
  asserts.isfalse(isfile(fullfile(destination, 'shared_app_a')));
  asserts.isfalse(isfolder(fullfile(destination, '.nelson-install')));
  invoke({fullfile(runtime, '.nelson-runtime', 'uninstall')}, true);
  asserts.isfalse(isfile(registration));
  clear restore;
end

function output = invoke(commandParts, success)
  quote = char(39);
  for k = 1:numel(commandParts)
    commandParts{k} = [quote, strrep(commandParts{k}, quote, char([39 34 39 34 39])), quote];
  end
  [status, output] = system([strjoin(commandParts, ' '), ' 2>&1'], 180);
  asserts.isequal(status == 0, success, output);
end

function restore_environment(names, values)
  for k = 1:numel(names); setenv(names{k}, values{k}); end
end
