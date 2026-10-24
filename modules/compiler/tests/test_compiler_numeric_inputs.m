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
ncc('--help');
check_inputs();

function check_inputs()
  work = [tempname(), ' numeric ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  source = fullfile(work, 'source');
  mkdir(source);
  for name = {'numeric_entry.m', 'str2double.m'}
    copyfile(fullfile(modulepath('compiler'), 'tests', 'fixtures', 'numeric_inputs', name{1}), source);
  end
  entry = fullfile(source, 'numeric_entry.m');
  shadow = fullfile(source, 'str2double.m');
  helpFile = fullfile(source, 'help.txt');
  filewrite(helpFile, 'NUMERIC_HELP_OK');
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  options = compiler.build.StandaloneApplicationOptions(entry, 'OutputDir', fullfile(work, 'plain'), ...
    'AdditionalFiles', shadow, 'CustomHelpTextFile', helpFile);
  plain = compiler.build.standaloneApplication(options);
  options.TreatInputsAsNumeric = 'on';
  options.OutputDir = fullfile(work, 'numeric');
  numeric = compiler.build.standaloneApplication(options);
  asserts.istrue(numeric.Options.TreatInputsAsNumeric);
  snapshot = numeric.__nelsonc_report__();
  asserts.isequal(snapshot.Manifest.format, 2);
  asserts.istrue(snapshot.Manifest.treatInputsAsNumeric);
  asserts.istrue(snapshot.DependencyPlan.treatInputsAsNumeric);
  asserts.istrue(contains(fileread(numeric.Files{2}), 'str2double'));
  a = plain.RuntimeDependencies.__nelsonc_snapshot__();
  b = numeric.RuntimeDependencies.__nelsonc_snapshot__();
  asserts.isequal(a.modules, b.modules);
  asserts.isequal({a.files.relative}, {b.files.relative});
  options.OutputDir = fullfile(work, 'repeated');
  repeated = compiler.build.standaloneApplication(options);
  asserts.isequal(sha256(numeric.Files{1}, '-file'), sha256(repeated.Files{1}, '-file'));
  applications = {numeric.Files{1}};
  if ispc()
    options.OutputDir = fullfile(work, 'windowed');
    options.EmbedArchive = false;
    windowed = compiler.build.standaloneWindowsApplication(options);
    asserts.isequal(windowed.BuildType, 'standaloneWindowsApplication');
    applications{end + 1} = windowed.Files{1};
  end
  native = ncc('options', entry, 'OutputDir', fullfile(work, 'bundled'), ...
    'TreatInputsAsNumeric', true, 'AdditionalFiles', shadow, 'CustomHelpTextFile', helpFile);
  analyzed = nelson.compiler.analyze(native);
  asserts.istrue(analyzed.treatInputsAsNumeric);
  bundled = nelson.compiler.build(native);
  asserts.istrue(isfolder(bundled.RuntimeDirectory));
  applications{end + 1} = bundled.Executable;
  suffix = ''; if ispc(); suffix = '.exe'; end
  tool = fullfile(modulepath('nelson', 'bin'), ['nelsonc', suffix]);
  toolApp = fullfile(work, ['native_command', suffix]);
  [status, output] = system(['"', tool, '" "', entry, ...
    '" --cli --numeric-inputs --runtime installed -o "', toolApp, '"'], 90);
  asserts.isequal(status, 0, output);
  applications{end + 1} = toolApp;
  check_manifest(numeric, work, suffix);
  script = fullfile(source, 'script_entry.m');
  filewrite(script, 'values = argv(''user''); if ~iscell(values) || ~ischar(values{1}); error(''Script argv changed.''); end; disp(''SCRIPT_INPUTS_OK'');');
  scriptApp = fullfile(work, ['script_app', suffix]);
  ncc(script, '-n', '--cli', '--runtime', 'installed', '-o', scriptApp);
  rmdir(source, 's');
  args = numeric_arguments();
  execute(plain.Files{1}, args, 'CHAR_INPUTS_OK');
  for k = 1:numel(applications)
    execute(applications{k}, args, 'NUMERIC_INPUTS_OK');
    execute(applications{k}, '', 'NO_INPUTS_OK');
  end
  execute(numeric.Files{1}, '--help', 'NUMERIC_HELP_OK');
  execute(scriptApp, '42', 'SCRIPT_INPUTS_OK');
  clear restore cleanup;
end

function args = numeric_arguments()
  values = {'3.25', '-2.5e2', '1 + 2i', '', 'NaN', 'Inf', '-Inf', '[]', '[1 2]', ...
    '1:3', '1+2', 'error(''INPUT_EXECUTED'')', char([28450, 23383]), '42tail', '--help', '  17  ', '1,200', '-3j'};
  for k = 1:numel(values); values{k} = ['"', values{k}, '"']; end
  args = strjoin(values, ' ');
end

function execute(executable, args, marker)
  [status, output] = system(['"', executable, '" ', args, ' 2>&1'], 60);
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, marker), output);
end

function check_manifest(result, work, suffix)
  extracted = fullfile(work, 'extracted');
  __application_archive__('directory', extracted);
  __application_archive__('extract', result.Files{1}, extracted);
  original = jsondecode(fileread(fullfile(extracted, 'application.json')));
  stub = fullfile(modulepath('nelson', 'bin'), ['nelson-app', suffix]);
  for k = 1:6
    manifest = original;
    switch k
      case 1; manifest = rmfield(manifest, 'treatInputsAsNumeric');
      case 2; manifest.treatInputsAsNumeric = 1;
      case 3; manifest.treatInputsAsNumeric = 'on';
      case 4; manifest.treatInputsAsNumeric = [true false];
      case 5; manifest.format = 1;
      case 6; manifest.format = 3;
    end
    filewrite(fullfile(extracted, 'application.json'), jsonencode(manifest));
    archive = fullfile(work, ['malformed', int2str(k), '.zip']);
    app = fullfile(work, ['malformed', int2str(k), suffix]);
    __nelsonc_archive__('pack', archive, {'application.json', 'roots'}, [extracted, '/']);
    __nelsonc_archive__('create-installed', stub, archive, app, false, manifest.engineFingerprint);
    [status, output] = system(['"', app, '" 3.25 2>&1'], 60);
    asserts.isequal(status, 1, output);
    asserts.isfalse(contains(output, '_INPUTS_OK'), output);
    asserts.istrue(contains(output, 'manifest') || contains(output, 'input conversion'), output);
  end
end
