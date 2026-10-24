%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_linux_installer__(stage, payload, shortcut, options, report, plan)
  runtime = '';
  if strcmp(options.RuntimeDelivery, 'installer') && report.launcher.runtimeDiscoveryVersion >= 3
    [parent, name] = fileparts(report.executable);
    runtime = fullfile(parent, [name, '.runtime']);
    package = fullfile(stage, 'runtime-package'); mkdir(package);
    original = fullfile(payload, runtime);
    movefile(original, fullfile(package, 'payload'));
    __nelsonc_linux_runtime_package__(package, plan);
    movefile(fullfile(package, 'payload'), original);
  end
  resource = fullfile(modulepath('compiler'), 'resources', 'linux', 'build.sh');
  arguments = {resource, payload, stage, options.InstallerName, ...
    options.DefaultInstallationDir, shortcut, options.ApplicationName, runtime, report.engineFingerprint};
  command = '/bin/bash';
  for k = 1:numel(arguments)
    command = [command, ' ', shell_argument(arguments{k})];
  end
  [status, output] = system(command, 1800);
  if options.Verbose || status ~= 0; fprintf('%s\n', output); end
  if status ~= 0
    error('Nelson:nelsonc:installerTool:linuxInstallerGenerationFailed', 'Linux installer generation failed with status %d.', status);
  end
  publish = fullfile(stage, 'publish');
  name = [options.InstallerName, '.install'];
  if ~isfile(fullfile(publish, name))
    error('Nelson:nelsonc:installerTool:installerGenerationProducedNoExecutable', 'Installer generation produced no executable.');
  end
  if ~isfolder(options.OutputDir); mkdir(options.OutputDir); end
  __nelsonc_executable__('publish', publish, options.OutputDir);
  if options.Verbose; fprintf('Created %s\n', fullfile(options.OutputDir, name)); end
end

function text = shell_argument(value)
  quote = char(39);
  text = [quote, strrep(value, quote, char([39 34 39 34 39])), quote];
end
