%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_build_dependencies(MODULE_PATH)
  module_json = [MODULE_PATH, '/module.json'];
  if ~isfile(module_json)
    error(_('module.json is missing.'));
  end
  data = jsondecode(module_json, '-file');
  if any(contains(fieldnames(data), 'dependencies'))
    dependencies = data.dependencies;
    names = fieldnames(dependencies);
    for n = names'
      r = nmm_is_http_repository(dependencies.(n{1}));
      if r || isdir(dependencies.(n{1}))
        nmm('install', dependencies.(n{1}))
      else
        nmm('install', n, dependencies.(n{1}))
      end
    end
  end
end
%=============================================================================
