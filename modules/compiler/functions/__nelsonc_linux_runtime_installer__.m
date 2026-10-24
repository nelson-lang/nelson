%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_linux_runtime_installer__(stage, payload, plan, options)
  package = fullfile(stage, 'package'); mkdir(package);
  destination = fullfile(package, 'payload'); movefile(payload, destination);
  rmfile(fullfile(destination, 'runtime.json'));
  rmfile(fullfile(destination, 'modules', 'modules.m'));
  __nelsonc_linux_runtime_package__(package, plan);
  build = fullfile(modulepath('compiler'), 'resources', 'linux', 'build-runtime.sh');
  invoke({'/bin/bash', build, package, stage, options.InstallerName, plan.engineFingerprint});
  if ~isfolder(options.OutputDir); mkdir(options.OutputDir); end
  __nelsonc_executable__('publish', fullfile(stage, 'publish'), options.OutputDir);
end

function invoke(commandParts)
  quote = char(39);
  for k = 1:numel(commandParts)
    commandParts{k} = [quote, strrep(commandParts{k}, quote, char([39 34 39 34 39])), quote];
  end
  [status, output] = system(strjoin(commandParts, ' '), 1800);
  if status ~= 0
    error('Nelson:nelsonc:installerTool:linuxRuntimeInstallerGenerationFailed', 'Linux runtime installer generation failed: %s', output);
  end
end
