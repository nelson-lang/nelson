%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_linux_runtime_package__(package, plan)
  tool = fullfile(modulepath('nelson', 'bin'), 'nelson-runtime-installer');
  if ~isfile(tool)
    error('Nelson:nelsonc:installerTool:linuxRuntimeInstallerToolMissing', 'The native Linux runtime installer tool is missing. Rebuild or install the compiler module.');
  end
  files = rmfield(plan.runtime.files, 'source');
  for k = 1:numel(files); files(k).mode = 420; end
  inventory = struct('format', 1, 'kind', 'nelson-runtime-inventory', ...
    'architecture', plan.architecture, 'runtimeVersion', plan.runtimeVersion, ...
    'engineFingerprint', plan.engineFingerprint, 'startupOrder', {plan.runtime.startupOrder}, ...
    'moduleOrder', {plan.runtime.moduleOrder}, 'flags', struct('gui', plan.runtime.gui, ...
    'dynamicStartup', plan.runtime.dynamicStartup), 'files', files, 'directories', {plan.runtime.directories});
  filewrite(fullfile(package, 'runtime-package.json'), jsonencode(inventory));
  helper = fullfile(package, 'runtime-installer');
  expected = sha256(tool, '-file'); copyfile(tool, helper);
  if ~strcmp(sha256(helper, '-file'), expected)
    error('Nelson:nelsonc:installerTool:runtimeInstallerToolChangedDuring', 'Runtime installer tool changed during packaging.');
  end
  quote = char(39);
  quoted = @(value)[quote, strrep(value, quote, char([39 34 39 34 39])), quote];
  [status, output] = system([quoted(helper), ' --prepare ', quoted(package)], 1800);
  if status ~= 0
    error('Nelson:nelsonc:installerTool:linuxRuntimePreparationFailed', 'Linux runtime preparation failed: %s', output);
  end
end
