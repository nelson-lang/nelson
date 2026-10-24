%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function packages = nmm_normalize_packages(packages)
  % Registry entries as one homogeneous struct array.
  % jsondecode returns a cell array when the entries do not share the same
  % fields (optional keys such as "tests", "yanked" or "homepage" on some
  % entries only), and a struct array cannot receive an entry with different
  % fields: rebuild every entry over the union of the fields (missing ones are
  % []) so consumers can always index packages(k).name.
  if isstruct(packages)
    if isempty(packages)
      return
    end
    packages = num2cell(packages);
  end
  if ~iscell(packages)
    packages = struct([]);
    return
  end
  names = {};
  for k = 1:numel(packages)
    if isstruct(packages{k})
      for f = fieldnames(packages{k})'
        if ~any(strcmp(names, f{1}))
          names{end + 1} = f{1};
        end
      end
    end
  end
  normalized = struct([]);
  for k = 1:numel(packages)
    if ~isstruct(packages{k})
      continue
    end
    item = struct();
    for f = names
      if isfield(packages{k}, f{1})
        item.(f{1}) = packages{k}.(f{1});
      else
        item.(f{1}) = [];
      end
    end
    if isempty(normalized)
      normalized = item;
    else
      normalized(end + 1) = item;
    end
  end
  packages = normalized;
end
%=============================================================================
