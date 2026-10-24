%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function encoded = __nelsonc_build_report__(report, options, files, stage, packages)
  entries = cell(1, numel(files));
  for k = 1:numel(files)
    [parent, stem, extension] = fileparts(files{k});
    name = [stem, extension];
    entries{k} = struct('relative', name, 'sha256', sha256(fullfile(stage, name), '-file'));
  end
  [parent, stem, extension] = fileparts(report.Executable);
  document = struct('format', 2, 'kind', 'nelson-standalone-build', ...
    'architecture', computer('arch'), 'runtimeVersion', report.DependencyPlan.runtimeVersion, ...
    'engineFingerprint', report.DependencyPlan.engineFingerprint, ...
    'applicationName', options.ExecutableName, 'executableVersion', options.ExecutableVersion, ...
    'buildType', report.BuildType, 'executable', [stem, extension], 'files', {entries}, ...
    'includedSupportPackages', {packages}, 'runtime', report.RuntimePlan, 'launcher', report.Launcher);
  encoded = __nelsonc_plan__('build-report', jsonencode(document));
end
