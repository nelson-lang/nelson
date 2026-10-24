%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function manifest = __nelsonc_stage_application__(plan, payload, gui)
  roots = plan.searchRoots;
  if isstring(roots)
    roots = cellstr(roots);
  end
  manifest = struct('format', 1, 'entry', '', 'entryKind', plan.entryKind, ...
    'entryName', '', 'gui', gui, 'paths', {{}}, 'engineFingerprint', plan.engineFingerprint, ...
    'builtinCatalogDigest', plan.builtinCatalogDigest);
  manifest.builtinCatalog = plan.builtinCatalog;
  manifest.nativePaths = {};
  [payloadRoots, manifest.paths] = __nelsonc_stage_paths__(plan, payload);
  for k = 1:numel(plan.files)
    item = plan.files(k);
    if ~isempty(item.module)
      continue;
    end
    if strcmp(item.source, plan.entry)
      manifest.entryName = entry_name(item.relative);
    end
    index = find(strcmp(roots, item.root), 1);
    if isempty(index)
      error('Nelson:nelsonc:internal', 'Missing application search root.');
    end
    relative = [payloadRoots{index}, '/', item.relative];
    if strcmp(item.representation, 'native')
      folder = strrep(fileparts(relative), '\', '/');
      if ~any(strcmp(manifest.nativePaths, folder))
        manifest.nativePaths{end + 1} = folder;
      end
    end
    destination = fullfile(payload, relative);
    [parent, name, extension] = fileparts(destination);
    if ~isfolder(parent)
      mkdir(parent);
    end
    compileSource = strcmp(item.representation, 'bytecode') && strcmp(extension, '.m');
    compiledDestination = fullfile(parent, [name, '.nbc']);
    if isfile(destination) || isfolder(destination) ...
      || (compileSource && (isfile(compiledDestination) || isfolder(compiledDestination)))
      error('Nelson:nelsonc:inputCollision', 'Application inputs share a packaged path: %s.', relative);
    end
    [success, message] = copyfile(item.source, destination);
    if ~success
      error('Nelson:nelsonc:copy', '%s: %s -> %s', message, item.source, destination);
    end
    if ~strcmpi(sha256(destination, '-file'), item.sha256)
      error('Nelson:nelsonc:sourceChanged', 'Application input changed after analysis: %s.', item.source);
    end
    if compileSource
      snapshot = destination;
      destination = compiledDestination;
      relative = [relative(1:end - 2), '.nbc'];
      __bytecode_artifact__('compile', snapshot, destination);
      delete(snapshot);
    end
    if strcmp(item.source, plan.entry)
      manifest.entry = relative;
    end
  end
  if isempty(manifest.entry)
    error('Nelson:nelsonc:entry', 'Missing application entry.');
  end
end

function qualified = entry_name(relative)
  [parent, name] = fileparts(relative);
  components = strsplit(strrep(parent, '\', '/'), '/');
  names = {};
  for k = 1:numel(components)
    if startsWith(components{k}, '+')
      names{end + 1} = components{k}(2:end);
    end
  end
  names{end + 1} = name;
  qualified = strjoin(names, '.');
  if isempty(regexp(qualified, '^[A-Za-z_][A-Za-z_0-9]*(\.[A-Za-z_][A-Za-z_0-9]*)*$', 'once'))
    error('Nelson:nelsonc:entry:invalidApplicationEntryName', 'Invalid application entry name: %s.', qualified);
  end
end
