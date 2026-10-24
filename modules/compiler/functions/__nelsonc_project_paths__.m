%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function project = __nelsonc_project_paths__(project, folder, storing)
  folder = normalized_path(folder);
  project.LastBuild.ReportFile = project_path(project.LastBuild.ReportFile, folder, storing);
  groups = {'Build', 'Installer'};
  keys = {{'AppFile', 'AdditionalFiles', 'CustomHelpTextFile', 'ExecutableIcon', 'OutputDir'}, ...
    {'AdditionalFiles', 'InstallerIcon', 'InstallerLogo', 'AddRemoveProgramsIcon', 'OutputDir', 'Shortcut'}};
  for j = 1:numel(groups)
    group = groups{j};
    names = keys{j};
    for k = 1:numel(names)
      key = names{k};
      value = project.(group).(key);
      if strcmp(key, 'AdditionalFiles')
        if isstring(value); value = cellstr(value); end
        if iscell(value) && (isempty(value) || isvector(value))
          for n = 1:numel(value); value{n} = project_path(value{n}, folder, storing); end
        else
          value = project_path(value, folder, storing);
        end
      else
        value = project_path(value, folder, storing);
      end
      project.(group).(key) = value;
    end
  end
end

function value = project_path(value, folder, storing)
  if isstring(value) && isscalar(value); value = char(value); end
  % Invalid types are rejected by the shared option validators after resolution.
  if ~ischar(value) || isempty(value) || ~isrow(value); return; end
  if any(value == char(0))
    error('Nelson:compiler:project:pathNullCharacters', 'Project paths cannot contain null characters.');
  end
  if ispc(); value = strrep(value, '\', '/'); end
  if storing
    value = stored_path(value, folder);
  else
    if ispc() && numel(value) >= 2 && value(2) == ':'
      % Leave drive-relative paths for the shared validator to reject.
      if numel(value) < 3 || value(3) ~= '/'; return; end
    elseif ispc() && startsWith(value, '/') && ~startsWith(value, '//')
      if numel(folder) < 2 || folder(2) ~= ':'
        error('Nelson:compiler:project:driveQualifiedPathRequired', 'A drive-qualified project path is required.');
      end
      value = [folder(1:2), value];
    elseif ~startsWith(value, '/')
      value = [folder, '/', value];
    end
    value = normalized_path(value);
  end
end

function value = stored_path(value, folder)
  value = normalized_path(value);
  prefix = folder;
  if ~endsWith(prefix, '/'); prefix = [prefix, '/']; end
  pathKey = value;
  folderKey = folder;
  prefixKey = prefix;
  if ispc()
    pathKey = lower(pathKey);
    folderKey = lower(folderKey);
    prefixKey = lower(prefixKey);
  end
  if strcmp(pathKey, folderKey) || strcmp(pathKey, prefixKey)
    value = '.';
  elseif startsWith(pathKey, prefixKey)
    value = ['./', value(numel(prefix) + 1:end)];
  end
end

function value = normalized_path(value)
  % An absolute path against a relative base is normalized without following links.
  value = relativepath('.', value);
  driveRoot = ispc() && numel(value) == 3 && value(2) == ':';
  if numel(value) > 1 && endsWith(value, '/') && ~driveRoot
    value(end) = [];
  end
end
