%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = __nelsonc_standalone__(noConsole, entry, varargin)
  if isa(entry, 'compiler.build.StandaloneApplicationOptions')
    if ~isscalar(entry) || ~isempty(varargin)
      error('Nelson:nelsonc:options:scalarOptionsObjectWithoutOverrides', 'Pass one scalar options object without name-value overrides.');
    end
    options = entry;
  else
    options = compiler.build.StandaloneApplicationOptions(entry, varargin{:});
  end
  settings = rmfield(struct(options), 'AppFile');
  native = nelson.compiler.BuildOptions(options.AppFile, settings);
  native.RuntimeMode = 'installed';
  native.NoConsole = noConsole;
  native.Verbose = false;
  suffix = '';
  if ispc()
    suffix = '.exe';
  end
  filename = [options.ExecutableName, suffix];
  files = {fullfile(options.OutputDir, filename); fullfile(options.OutputDir, 'readme.txt')};
  if ~options.EmbedArchive
    files = [files(1); {fullfile(options.OutputDir, [options.ExecutableName, '.nca'])}; files(2)];
  end
  outputs = [files; {fullfile(options.OutputDir, 'buildresult.json')}];
  previous = __nelsonc_previous_build__(options, outputs);
  stage = tempname();
  __application_archive__('directory', stage);
  cleanup = onCleanup(@() rmdir(stage, 's'));
  native.OutputDir = stage;
  built = nelson.compiler.build(native);
  report = struct(built);
  report.Launcher = jsondecode(__nelsonc_executable__('launcher-contract', report.Executable));
  report.RuntimePlan = __nelsonc_runtime_plan__(report.DependencyPlan, report.RuntimePlan.gui);
  report.RuntimePlan.mode = 'installed';
  packages = report.DependencyPlan.includedSupportPackages;
  filewrite(fullfile(stage, 'readme.txt'), readme_text(report, filename));
  native.OutputDir = options.OutputDir;
  native.Verbose = options.Verbose;
  report.Options = struct(native);
  report.Executable = files{1};
  report.Files = files;
  encoded = __nelsonc_build_report__(report, options, files, stage, packages);
  report.Distribution = jsondecode(encoded);
  filewrite(fullfile(stage, 'buildresult.json'), encoded);
  result = compiler.build.Results(report, options, files, packages);
  if ~isfolder(options.OutputDir)
    mkdir(options.OutputDir);
  end
  __nelsonc_executable__('publish', stage, options.OutputDir, previous);
  if options.Verbose
    fprintf('Created %s\n', files{1});
    fprintf('Required runtime modules: %d\n', numel(report.RuntimePlan.modules));
  end
  clear cleanup;
end

function text = readme_text(report, filename)
  inputText = 'Command-line inputs are passed to the entry function as character vectors.';
  if report.Options.TreatInputsAsNumeric
    inputText = 'Command-line inputs are converted with str2double; invalid values become NaN. No code is evaluated.';
  end
  archiveText = 'Application code and selected data files are embedded in the executable.';
  if ~report.Options.EmbedArchive
    [parent, name] = fileparts(filename);
    archiveText = ['Keep ', name, '.nca beside the executable; its size and SHA-256 are verified before execution.'];
  end
  lines = {'Nelson standalone application', filename, '', ...
    ['Runtime version: ', report.DependencyPlan.runtimeVersion], ...
    ['Architecture: ', computer('arch')], ...
    ['Engine fingerprint: ', report.DependencyPlan.engineFingerprint], '', ...
    'This build does not include a runtime or an installer.', ...
    'Install a compatible Nelson runtime on the destination machine.', ...
    'Set NELSONC_RUNTIME_ROOT to its installation directory before launching.', ...
    'A runtime with a different engine fingerprint is rejected.', '', ...
    archiveText, ...
    'Application data is extracted to a private temporary directory at startup.', ...
    'Do not store persistent output in ctfroot; it is removed at normal shutdown.', ...
    inputText, ...
    'The application is native to the build platform, not cross-compiled.', ''};
  text = strjoin(lines, char(10));
end
