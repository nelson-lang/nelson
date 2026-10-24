%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function records = __nelsonc_mac_inventory__(root)
  root = fullpath(root);
  [status, links] = system(['/usr/bin/find ', shell_argument(root), ' -type l -print -quit']);
  if status ~= 0 || ~isempty(strtrim(links))
    error('Nelson:nelsonc:installer:symbolicLinksNotSupportedMacOS', 'Symbolic links are not supported in macOS package payloads.');
  end
  [status, output] = system(['/usr/bin/find ', shell_argument(root), ' -type f -perm -111 -print']);
  if status ~= 0; error('Nelson:nelsonc:installer:cannotInspectExecutablePackageInputs', 'Cannot inspect executable package inputs.'); end
  executable = strsplit(strtrim(output), char(10));
  if isempty(output); executable = {}; end
  normalize_modes(root, executable);
  records = collect(root, '', executable);
  [~, order] = sort({records.relative});
  records = records(order);
end

function records = collect(root, relative, executable)
  records = struct('kind', {}, 'sha256', {}, 'mode', {}, 'relative', {});
  folder = root;
  if ~isempty(relative); folder = fullfile(root, relative); end
  contents = dir(folder);
  for k = 1:numel(contents)
    name = contents(k).name;
    if strcmp(name, '.') || strcmp(name, '..'); continue; end
    child = name;
    if ~isempty(relative); child = [relative, '/', name]; end
    validate_relative(child);
    filename = fullfile(root, child);
    if contents(k).isdir
      records(end + 1) = struct('kind', 'D', 'sha256', '-', 'mode', '755', 'relative', child);
      records = [records, collect(root, child, executable)];
    else
      mode = '644';
      if any(strcmp(executable, filename)); mode = '755'; end
      records(end + 1) = struct('kind', 'F', 'sha256', sha256(filename, '-file'), ...
        'mode', mode, 'relative', child);
    end
  end
end

function normalize_modes(root, executable)
  [status, output] = system(['/usr/bin/find ', shell_argument(root), ...
    ' -type d -exec /bin/chmod 755 {} + -o -type f -exec /bin/chmod 644 {} +']);
  if status ~= 0; error('Nelson:nelsonc:installer:cannotNormalizePackageModes', 'Cannot normalize package modes: %s', output); end
  if isempty(executable); return; end
  parts = {'/bin/chmod', '755'};
  for k = 1:numel(executable); parts{end + 1} = shell_argument(executable{k}); end
  [status, output] = system(strjoin(parts, ' '));
  if status ~= 0; error('Nelson:nelsonc:installer:cannotRestoreExecutableModes', 'Cannot restore executable modes: %s', output); end
end

function validate_relative(value)
  parts = strsplit(value, '/');
  if isempty(value) || startsWith(value, '/') || contains(value, '\') || contains(value, '//') ...
    || any(value < char(32)) || any(strcmp(parts, '')) || any(strcmp(parts, '.')) ...
    || any(strcmp(parts, '..'))
    error('Nelson:nelsonc:installer:unsupportedMacOSPackagePath', 'Unsupported macOS package path: %s.', value);
  end
end

function value = shell_argument(value)
  quote = char(39);
  value = [quote, strrep(value, quote, char([39 34 39 34 39])), quote];
end
