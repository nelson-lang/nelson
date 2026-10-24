%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_mark_installed_as_dependency(dependencyName)
  modules = nmm_list();
  if ~isfield(modules, dependencyName)
    return
  end
  data = modules.(dependencyName);
  data.installed_as_dependency = true;
  if isfield(data, 'versions') && ~isempty(data.versions)
    for k = 1:length(data.versions)
      if strcmp(data.versions(k).version, data.version)
        data.versions(k).installed_as_dependency = true;
      end
    end
  end
  modules.(dependencyName) = data;
  filewrite([usermodulesdir(), 'modules.json'], jsonprettyprint(jsonencode(modules)));
end
%=============================================================================
