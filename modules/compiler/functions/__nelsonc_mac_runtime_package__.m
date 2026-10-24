%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_mac_runtime_package__(package, plan)
  tool = fullfile(modulepath('nelson', 'bin'), 'nelson-runtime-installer');
  if ~isfile(tool)
    error('Nelson:nelsonc:installerTool:macOSRuntimeInstallerToolMissing', 'The native macOS runtime installer tool is missing. Rebuild or install the compiler module.');
  end
  files = rmfield(plan.runtime.files, 'source');
  for k = 1:numel(files); files(k).mode = 420; end
  inventory = struct('format', 1, 'kind', 'nelson-runtime-inventory', ...
    'architecture', plan.architecture, 'runtimeVersion', plan.runtimeVersion, ...
    'engineFingerprint', plan.engineFingerprint, 'startupOrder', {plan.runtime.startupOrder}, ...
    'moduleOrder', {plan.runtime.moduleOrder}, 'flags', struct('gui', plan.runtime.gui, ...
    'dynamicStartup', plan.runtime.dynamicStartup), 'files', files, 'directories', {plan.runtime.directories});
  filewrite(fullfile(package, 'runtime-package.json'), jsonencode(inventory));
  helper = fullfile(package, 'runtime-installer'); copyfile(tool, helper);
  set_mode(helper, '755');
  [status, canonical] = system(['/bin/realpath ', shell_argument(package)]);
  if status ~= 0
    error('Nelson:nelsonc:installerTool:cannotResolveMacOSRuntimeStaging', 'Cannot resolve the macOS runtime staging path.');
  end
  package = strtrim(canonical);
  invoke({fullfile(package, 'runtime-installer'), '--prepare', package});
end

function invoke(parts)
  for k = 1:numel(parts); parts{k} = shell_argument(parts{k}); end
  [status, output] = system(strjoin(parts, ' '), 1800);
  if status ~= 0
    error('Nelson:nelsonc:installerTool:macOSRuntimePreparationFailed', 'macOS runtime preparation failed: %s', output);
  end
end

function set_mode(filename, mode)
  [status, output] = system(['/bin/chmod ', mode, ' ', shell_argument(filename)]);
  if status ~= 0; error('Nelson:nelsonc:installerTool:cannotSetRuntimeHelperMode', 'Cannot set runtime helper mode: %s', output); end
end

function value = shell_argument(value)
  quote = char(39); value = [quote, strrep(value, quote, char([39 34 39 34 39])), quote];
end
