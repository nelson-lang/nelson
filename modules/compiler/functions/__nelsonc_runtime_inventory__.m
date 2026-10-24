%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_runtime_inventory__(plan, filename)
  runtime = plan.runtime;
  files = cell(1, numel(runtime.files));
  for k = 1:numel(runtime.files)
    item = runtime.files(k);
    if any(strcmpi(item.relative, {'runtime-install.ini', 'runtime-package.ini'})) ...
      || startsWith(lower(item.relative), 'unins') || startsWith(lower(item.relative), '.nelson-runtime-')
      error('Nelson:nelsonc:runtimeCollision:runtimeFileCollidesInstallerMetadata', 'Runtime file collides with installer metadata.');
    end
    files{k} = [item.relative, '|', item.sha256];
  end
  for k = 1:numel(runtime.directories)
    if startsWith(lower(runtime.directories{k}), '.nelson-runtime-')
      error('Nelson:nelsonc:runtimeCollision:runtimeDirectoryCollidesInstallerRecovery', 'Runtime directory collides with installer recovery metadata.');
    end
  end
  lines = {'[Identity]', 'Format=1', ['Architecture=', plan.architecture], ...
    ['Version=', plan.runtimeVersion], ['Engine=', plan.engineFingerprint], ...
    ['GUI=', num2str(double(runtime.gui))], ['Dynamic=', num2str(double(runtime.dynamicStartup))]};
  lines = [lines, section('Startup', runtime.startupOrder), section('Modules', runtime.moduleOrder), ...
    section('Files', files), section('Directories', runtime.directories)];
  filewrite(filename, [char(65279), strjoin(lines, char(10))], 'pc', 'UTF-16LE');
end

function lines = section(name, values)
  lines = {['[', name, ']'], ['Count=', num2str(numel(values))]};
  for k = 1:numel(values)
    value = values{k};
    if any(value < char(32)) || startsWith(value, '"') || startsWith(value, '''')
      error('Nelson:nelsonc:runtimePath:invalidRuntimeInventoryValue', 'Invalid runtime inventory value.');
    end
    lines{end + 1} = [num2str(k - 1), '=', value];
  end
end
