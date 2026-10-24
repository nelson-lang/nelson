%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function packages = __nelsonc_package_registry__(registry)
  if nargin < 1; registry = nmm('list'); end
  packages = struct('Name', {}, 'Root', {});
  if isempty(registry); return; end
  if ~isstruct(registry) || ~isscalar(registry)
    error('Nelson:nelsonc:packages:invalidInstalledPackageRegistry', 'Invalid installed package registry.');
  end
  names = sort(fieldnames(registry));
  for k = 1:numel(names)
    record = registry.(names{k});
    if ~isstruct(record) || ~isscalar(record)
      error('Nelson:nelsonc:packages:invalidInstalledPackageRecord', 'Invalid installed package record: %s.', names{k});
    end
    versions = record;
    if isfield(record, 'versions') && ~isempty(record.versions); versions = record.versions; end
    if ~isstruct(versions) || ~isfield(versions, 'path')
      error('Nelson:nelsonc:packages:missingInstalledPackagePath', 'Missing installed package path: %s.', names{k});
    end
    paths = {versions.path};
    if isfield(record, 'path'); paths = [{record.path}, paths]; end
    for j = 1:numel(paths)
      root = paths{j};
      if ~ischar(root) || ~isrow(root) || isempty(root) || any(root == char(0))
        error('Nelson:nelsonc:packages:invalidInstalledPackagePath', 'Invalid installed package path: %s.', names{k});
      end
      if ~isfolder(root); continue; end
      root = strrep(fullpath(root), '\', '/');
      if any(strcmp({packages.Name}, names{k}) & strcmp({packages.Root}, root)); continue; end
      packages(end + 1) = struct('Name', names{k}, 'Root', root);
    end
  end
end
