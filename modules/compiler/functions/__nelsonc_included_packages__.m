%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function packages = __nelsonc_included_packages__(plan, registry)
  if nargin < 2; registry = nmm('list'); end
  catalog = __nelsonc_package_registry__(registry);
  packages = {};
  sources = {plan.files.source};
  for k = 1:numel(sources); sources{k} = normalized_path(sources{k}); end
  for k = 1:numel(catalog)
    root = normalized_path(catalog(k).Root);
    if ~endsWith(root, '/'); root = [root, '/']; end
    if any(startsWith(sources, root)) && ~any(strcmp(packages, catalog(k).Name))
      packages{end + 1} = catalog(k).Name;
    end
  end
end

function value = normalized_path(value)
  value = strrep(fullpath(value), '\', '/');
  if ispc(); value = lower(value); end
end
