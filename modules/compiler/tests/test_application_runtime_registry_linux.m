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
% <--TIMEOUT 300-->
if ispc() || ismac(); return; end
ncc('--help');
check_registry();

function check_registry()
  work = [tempname(), ' registry '' ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  entry = fullfile(work, 'registered_app.m');
  filewrite(entry, ['function registered_app(); if ~isdeployed() || sin(0) ~= 0; ', ...
    'error(''Invalid deployed computation.''); end; disp(''REGISTERED_RUNTIME_OK''); disp(nelsonroot()); end']);
  built = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(work, 'build'));
  plan = __nelsonc_distribution_plan__(built);
  runtime = fullfile(work, 'shared runtime');
  __nelsonc_copy_runtime__(plan.runtime, runtime);
  rmfile(entry);
  executable = built.Files{1};
  variables = {'NELSONC_RUNTIME_ROOT', 'NELSON_RUNTIME_PATH', 'LD_LIBRARY_PATH', ...
    'PATH', 'HOME', 'XDG_CONFIG_HOME', 'XDG_CONFIG_DIRS'};
  previous = cellfun(@getenv, variables, 'UniformOutput', false);
  restore = onCleanup(@() restore_environment(variables, previous));
  home = fullfile(work, 'home'); mkdir(home);
  user = fullfile(work, 'user configuration');
  system = fullfile(work, 'system configuration');
  setenv('NELSONC_RUNTIME_ROOT', ''); setenv('NELSON_RUNTIME_PATH', '');
  setenv('LD_LIBRARY_PATH', ''); setenv('PATH', '/usr/bin:/bin');
  setenv('HOME', home); setenv('XDG_CONFIG_HOME', user); setenv('XDG_CONFIG_DIRS', system);
  run_application(executable, false);
  registration = write_registration(user, plan, runtime);
  output = run_application(executable, true);
  asserts.istrue(contains(output, runtime), output);
  index = find(endsWith({plan.runtime.files.relative}, '/libnlsInterpreter.so'));
  asserts.isequal(numel(index), 1);
  interpreter = fullfile(runtime, plan.runtime.files(index).relative);
  saved = fullfile(work, 'saved-interpreter');
  movefile(interpreter, saved);
  filewrite(interpreter, 'CHANGED');
  output = run_application(executable, false);
  asserts.istrue(contains(output, 'No compatible installed Nelson runtime'), output);
  rmfile(interpreter);
  invoke('/usr/bin/mkfifo', {interpreter});
  output = run_application(executable, false);
  asserts.istrue(contains(output, 'No compatible installed Nelson runtime'), output);
  invoke('/usr/bin/rm', {'--', interpreter});
  invoke('/usr/bin/ln', {'-s', saved, interpreter});
  run_application(executable, true);
  rmfile(interpreter); movefile(saved, interpreter);
  setenv('NELSONC_RUNTIME_ROOT', fullfile(work, 'missing runtime'));
  run_application(executable, false);
  setenv('NELSONC_RUNTIME_ROOT', '');
  adjacent = [executable, '.runtime']; mkdir(adjacent);
  run_application(executable, false);
  rmdir(adjacent);
  filewrite(registration, ['NELSON_RUNTIME_ROOT_2', newline(), runtime, newline()]);
  run_application(executable, false);
  write_registration(system, plan, runtime);
  run_application(executable, true);
  rmfile(registration);
  rmfile(fullfile(system, 'nelson', 'runtimes', plan.architecture, [plan.engineFingerprint, '.root']));
  write_registration(fullfile(home, '.config'), plan, runtime);
  setenv('XDG_CONFIG_HOME', '');
  run_application(executable, true);
  setenv('XDG_CONFIG_HOME', 'relative');
  run_application(executable, true);
  setenv('HOME', 'relative');
  run_application(executable, false);
  clear restore cleanup;
end

function filename = write_registration(directory, plan, runtime)
  folder = fullfile(directory, 'nelson', 'runtimes', plan.architecture);
  if ~isfolder(folder); mkdir(folder); end
  filename = fullfile(folder, [plan.engineFingerprint, '.root']);
  filewrite(filename, ['NELSON_RUNTIME_ROOT_1', newline(), runtime, newline()]);
end

function output = run_application(executable, success)
  quote = char(39);
  command = [quote, strrep(executable, quote, char([39 34 39 34 39])), quote, ' 2>&1'];
  [status, output] = system(command, 45);
  asserts.isequal(status == 0, success, output);
  if success; asserts.istrue(contains(output, 'REGISTERED_RUNTIME_OK'), output); end
end

function restore_environment(names, values)
  for k = 1:numel(names); setenv(names{k}, values{k}); end
end

function invoke(command, arguments)
  quote = char(39);
  values = [{command}, arguments];
  for k = 1:numel(values)
    values{k} = [quote, strrep(values{k}, quote, char([39 34 39 34 39])), quote];
  end
  [status, output] = system(strjoin(values, ' '));
  asserts.isequal(status, 0, output);
end
