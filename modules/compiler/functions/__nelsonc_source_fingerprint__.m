%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function digest = __nelsonc_source_fingerprint__(plan, helpText, iconDigest)
  % Hash packaged identities, not host locations, timestamps or diagnostic prose.
  files = cell(1, numel(plan.files));
  entry = '';
  modulesRoot = fullpath(fullfile(nelsonroot(), 'modules'));
  for k = 1:numel(plan.files)
    item = plan.files(k);
    if isempty(item.module)
      name = archive_path(item, plan.searchRoots);
    else
      relative = relativepath(modulesRoot, item.source);
      if startsWith(relative, './'); relative = relative(3:end); end
      if ~startsWith(relative, [item.module, '/'])
        error('Nelson:compiler:project:invalidRetainedSourcePath', 'Invalid retained module source path.');
      end
      name = ['module/', relative];
    end
    record = struct('path', name, 'representation', item.representation, 'sha256', item.sha256);
    files{k} = jsonencode(record);
    if strcmp(item.source, plan.entry); entry = name; end
  end
  directories = mapped_paths(plan, 'directories');
  paths = mapped_paths(plan, 'pathEntries');
  inputs = struct('format', 1, 'files', {sort(files)}, 'entry', entry, 'entryKind', plan.entryKind, ...
    'directories', {sort(directories)}, 'paths', {paths}, 'rootCount', numel(plan.searchRoots), ...
    'engineFingerprint', plan.engineFingerprint, 'builtinCatalogDigest', plan.builtinCatalogDigest, ...
    'dynamicFallback', plan.dynamicFallback, 'helpSHA256', sha256(helpText), 'iconSHA256', iconDigest);
  digest = sha256(jsonencode(inputs));
end

function names = mapped_paths(plan, field)
  names = {};
  if ~isfield(plan, field); return; end
  for k = 1:numel(plan.(field))
    names{end + 1} = archive_path(plan.(field)(k), plan.searchRoots);
  end
end

function name = archive_path(item, roots)
  index = find(strcmp(roots, item.root), 1);
  if isempty(index)
    error('Nelson:compiler:project:missingSourceSearchRoot', 'Missing source search root in the build snapshot.');
  end
  name = [int2str(index), '/', item.relative];
end
