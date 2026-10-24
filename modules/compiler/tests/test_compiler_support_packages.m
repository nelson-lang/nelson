%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 240-->
ncc('--help');
check_packages();

function check_packages()
  oldPath = path();
  restorePath = onCleanup(@() path(oldPath));
  work = tempname(); mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  oldRegistry = getenv('NELSON_EXTERNAL_MODULES_PATH');
  restoreRegistry = onCleanup(@() setenv('NELSON_EXTERNAL_MODULES_PATH', oldRegistry));
  registryPath = fullfile(work, 'registry'); mkdir(registryPath);
  setenv('NELSON_EXTERNAL_MODULES_PATH', registryPath);
  source = fullfile(work, 'source'); mkdir(source);
  alpha = fullfile(work, 'alpha'); mkdir(alpha);
  alphaFunctions = fullfile(alpha, 'functions'); mkdir(alphaFunctions);
  beta = fullfile(work, 'beta'); mkdir(beta);
  sibling = fullfile(work, 'alpha_other'); mkdir(sibling);
  unused = fullfile(work, 'unused'); mkdir(unused);
  filewrite(fullfile(alphaFunctions, 'support_alpha.m'), 'function value = support_alpha(); value = 7; end');
  filewrite(fullfile(beta, 'support_beta.m'), 'function value = support_beta(); value = 9; end');
  filewrite(fullfile(beta, 'unused_broken.m'), 'function unused_broken(); nonexistent_package_dependency(); end');
  filewrite(fullfile(sibling, 'support_neighbor.m'), 'function value = support_neighbor(); value = 3; end');
  addpath(alphaFunctions); addpath(beta); addpath(sibling);
  registry = struct('alpha', struct('path', alpha), 'beta', struct('path', beta), 'unused', struct('path', unused));
  registryFile = fullfile(registryPath, 'modules.json');
  filewrite(registryFile, jsonencode(registry));
  entry = fullfile(source, 'support_entry.m');
  filewrite(entry, ['function support_entry(); if ~isdeployed(); disp(support_beta()); end; ', ...
    'disp(support_alpha() + support_neighbor()); end']);
  options = nelson.compiler.BuildOptions(entry, 'RuntimeMode', 'installed');
  asserts.isequal(options.SupportPackages, {'autodetect'});
  all = nelson.compiler.analyze(options);
  asserts.istrue(all.complete);
  asserts.isequal(all.includedSupportPackages, {'alpha', 'beta'});
  options.SupportPackages = "alpha";
  selected = nelson.compiler.analyze(options);
  asserts.istrue(selected.complete);
  asserts.isequal(selected.includedSupportPackages, {'alpha'});
  asserts.isfalse(any(strcmp({selected.files.source}, strrep(fullfile(beta, 'support_beta.m'), '\', '/'))));
  edges = selected.edges;
  if isstruct(edges); edges = num2cell(edges); end
  asserts.istrue(any(cellfun(@(edge) strcmp(edge.kind, 'excluded-support-package'), edges)));
  asserts.istrue(any(endsWith({selected.files.source}, '/support_neighbor.m')));
  currentPath = path();
  rmpath(alphaFunctions); rmpath(beta);
  reducedPath = path();
  resolved = nelson.compiler.analyze(options);
  asserts.isequal(resolved.includedSupportPackages, {'alpha'});
  asserts.isequal(path(), reducedPath);
  path(currentPath);
  options.SupportPackages = 'none';
  empty = nelson.compiler.analyze(options);
  asserts.istrue(empty.complete);
  asserts.isequal(empty.includedSupportPackages, {});
  asserts.isfalse(any(endsWith({empty.files.source}, '/support_alpha.m')));
  options.AdditionalFiles = {fullfile(alphaFunctions, 'support_alpha.m')};
  rejected(@() nelson.compiler.analyze(options));
  options.AdditionalFiles = {unused};
  rejected(@() nelson.compiler.analyze(options));
  options.AdditionalFiles = {};
  options.SupportPackages = {'not_installed'};
  rejected(@() nelson.compiler.analyze(options));
  malformed = struct('roots', {{alpha}}, 'modules', {{'alpha'}}, 'lookup', {{'relative'}});
  rejected(@() __nelsonc_plan__('link', entry, {}, true, jsonencode(malformed)));
  for bad = {{}, '', [], 5, {'none', 'alpha'}, {'autodetect', 'none'}, {'bad/name'}, {'alpha', 2}}
    rejected(@() __nelsonc_option_value__('SupportPackages', bad{1}));
  end
  asserts.isequal(__nelsonc_option_value__('SupportPackages', ["beta", "alpha", "alpha"]), {'alpha', 'beta'});
  before = sha256(registryFile, '-file');
  built = compiler.build.standaloneApplication(entry, 'SupportPackages', {'alpha'}, 'OutputDir', fullfile(work, 'build'));
  asserts.isequal(built.IncludedSupportPackages, {'alpha'});
  report = built.__nelsonc_report__();
  asserts.isequal(report.Distribution.includedSupportPackages, {'alpha'});
  executables = check_executables(entry, work, built);
  asserts.isequal(sha256(registryFile, '-file'), before);
  check_project(entry, work);
  check_pruning(source, registryFile, registry, options);
  clear restorePath;
  rmdir(alpha, 's'); rmdir(beta, 's'); rmdir(sibling, 's'); rmdir(source, 's');
  oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
  restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  for k = 1:numel(executables)
    [status, output] = system(['"', executables{k}, '"'], 60);
    asserts.isequal(status, 0, output);
    asserts.istrue(contains(output, '10'), output);
  end
  missing = fullfile(work, 'none', ['support_entry', get_exe_extension()]);
  [status, output] = system(['"', missing, '"'], 60);
  asserts.istrue(status ~= 0, output);
  asserts.istrue(contains(output, 'support_alpha'), output);
  clear restoreRuntime; clear restoreRegistry; clear cleanup;
end

function executables = check_executables(entry, work, selected)
  executables = selected.Files(1);
  default = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(work, 'default'));
  auto = compiler.build.standaloneApplication(entry, 'SupportPackages', 'autodetect', 'OutputDir', fullfile(work, 'auto'));
  asserts.isequal(sha256(default.Files{1}, '-file'), sha256(auto.Files{1}, '-file'));
  none = compiler.build.standaloneApplication(entry, 'SupportPackages', 'none', 'OutputDir', fullfile(work, 'none'));
  asserts.isequal(none.IncludedSupportPackages, {});
  repeated = compiler.build.standaloneApplication(entry, 'SupportPackages', 'alpha', 'OutputDir', fullfile(work, 'repeat'));
  asserts.isequal(sha256(selected.Files{1}, '-file'), sha256(repeated.Files{1}, '-file'));
  if ispc()
    windowed = compiler.build.standaloneWindowsApplication(entry, 'SupportPackages', 'alpha', 'OutputDir', fullfile(work, 'windowed'));
    asserts.isequal(windowed.IncludedSupportPackages, {'alpha'});
    executables{end + 1} = windowed.Files{1};
  end
  options = ncc('options', entry, 'SupportPackages', 'alpha', 'OutputDir', fullfile(work, 'bundled'));
  bundled = nelson.compiler.build(options);
  executables{end + 1} = bundled.Executable;
  cli = fullfile(modulepath('nelson', 'bin'), ['nelsonc', get_exe_extension()]);
  output = fullfile(work, ['command', get_exe_extension()]);
  [status, text] = system(['"', cli, '" "', entry, '" -o "', output, ...
    '" --runtime installed --support-package alpha --support-package unused -a "', ...
    fullfile(work, 'alpha_other', 'support_neighbor.m'), '"'], 120);
  asserts.isequal(status, 0, text);
  executables{end + 1} = output;
  asserts.isequal(ncc(entry, '--explain-link', '--runtime', 'installed', ...
    '--support-package', 'alpha').includedSupportPackages, {'alpha'});
  rejected(@() ncc(entry, '--support-package'));
  rejected(@() ncc(entry, '--support-package', 'none', '--support-package', 'alpha', '--explain-link'));
end

function extension = get_exe_extension()
  extension = ''; if ispc(); extension = '.exe'; end
end

function check_pruning(source, registryFile, registry, options)
  % Excluded code must not be analyzed or retain its unique runtime modules.
  filewrite(fullfile(registry.beta.path, 'support_beta.m'), 'function value = support_beta(); value = fft([1 2]); end');
  filewrite(fullfile(source, 'prune_entry.m'), 'function prune_entry(); if ~isdeployed(); support_beta(); end; disp(1); end');
  options.AppFile = fullfile(source, 'prune_entry.m');
  options.SupportPackages = 'none';
  selected = nelson.compiler.analyze(options);
  asserts.istrue(selected.complete);
  asserts.isfalse(any(strcmp({selected.modules.name}, 'fftw')));
  options.SupportPackages = 'autodetect';
  included = nelson.compiler.analyze(options);
  asserts.istrue(any(strcmp({included.modules.name}, 'fftw')), 'The included helper must retain its FFTW dependency.');
  filewrite(fullfile(source, 'dynamic_entry.m'), 'function dynamic_entry(name); feval(name); end');
  options.AppFile = fullfile(source, 'dynamic_entry.m');
  options.SupportPackages = 'none';
  dynamic = nelson.compiler.analyze(options);
  asserts.istrue(dynamic.complete, jsonencode(dynamic.diagnostics));
  asserts.isfalse(any(endsWith({dynamic.files.source}, '/unused_broken.m')));
  % Old installed versions must follow the same policy as the active version.
  registry.beta.versions = [struct('path', registry.beta.path), struct('path', registry.alpha.path)];
  registry = rmfield(registry, 'alpha');
  filewrite(registryFile, jsonencode(registry));
  options.AppFile = fullfile(source, 'support_entry.m');
  versions = nelson.compiler.analyze(options);
  asserts.isfalse(any(endsWith({versions.files.source}, '/support_alpha.m')));
end

function check_project(entry, work)
  project = __nelsonc_project__('new');
  project.Build.AppFile = entry;
  project.Build.SupportPackages = {'alpha'};
  project = __nelsonc_project__('validate', project);
  filename = fullfile(work, 'packages.ncproj');
  __nelsonc_project__('save', project, filename);
  asserts.isequal(__nelsonc_project__('load', filename), project);
  asserts.istrue(contains(__nelsonc_project__('command', project), 'options.SupportPackages = {''alpha''};'));
  old = project; old.Version = 5; old.Build = rmfield(old.Build, 'SupportPackages');
  old.LastBuild.ReportFile = strrep(fullfile(work, 'buildresult.json'), '\', '/');
  old.LastBuild.ReportSHA256 = repmat('a', 1, 64);
  old.LastBuild.SettingsSHA256 = __nelsonc_project_result__('fingerprint', old);
  migrated = __nelsonc_project__('validate', old);
  asserts.isequal(migrated.Version, 6);
  asserts.isequal(migrated.Build.SupportPackages, {'autodetect'});
  asserts.isequal(migrated.LastBuild.SettingsSHA256, __nelsonc_project_result__('fingerprint', migrated));
  old.Build.Verbose = true;
  changed = __nelsonc_project__('validate', old);
  asserts.isequal(changed.LastBuild.SettingsSHA256, old.LastBuild.SettingsSHA256);
  asserts.isfalse(strcmp(changed.LastBuild.SettingsSHA256, __nelsonc_project_result__('fingerprint', changed)));
end

function rejected(callback)
  failed = false; try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end
