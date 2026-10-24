%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function names = get_builtin_help_modules()
  run(fullfile(nelsonroot(), 'modules', 'modules.m'), false);
  names = string(cellfun(@(item) item{1}, modules_list, 'UniformOutput', false));
  root = fullfile(nelsonroot(), 'modules');
  folders = dir(root);
  for k = 1:numel(folders)
    name = folders(k).name;
    if ~folders(k).isdir || any(strcmp(names, name))
      continue;
    end
    directory = fullfile(root, name);
    manifest = fullfile(directory, 'module.json');
    if ~isfile(manifest) || ~isfolder(fullfile(directory, 'help')) ...
        || ~isfile(fullfile(directory, 'etc', 'startup.m'))
      continue;
    end
    try
      metadata = jsondecode(manifest, '-file');
    catch
      continue;
    end
    if isstruct(metadata) && isscalar(metadata) && isfield(metadata, 'builtin') ...
        && isequal(metadata.builtin, true)
      names(end + 1, 1) = string(name);
    end
  end
end
