%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

function __nelsonc_copy_runtime__(runtime, destination)
  if ~isfield(runtime, 'files') || ~isfield(runtime, 'inputs') || ~isfield(runtime, 'moduleOrder')
    error('Nelson:nelsonc:runtimeSnapshot', 'Missing runtime file snapshot.');
  end
  for k = 1:numel(runtime.inputs)
    item = runtime.inputs(k);
    if ~strcmpi(sha256(item.source, '-file'), item.sha256)
      error('Nelson:nelsonc:runtimeChanged', 'Runtime source changed after analysis: %s.', item.source);
    end
  end
  validate_destinations(runtime.files, runtime.directories);
  if isfolder(destination)
    contents = dir(destination);
    names = {contents.name};
    if any(~strcmp(names, '.') & ~strcmp(names, '..'))
      error('Nelson:nelsonc:runtimeDestination', 'Runtime destination must be empty.');
    end
  elseif isfile(destination)
    error('Nelson:nelsonc:runtimeDestination:runtimeDestinationDirectory', 'Runtime destination must be a directory.');
  else
    mkdir(destination);
  end
  for k = 1:numel(runtime.directories)
    mkdir(fullfile(destination, runtime.directories{k}));
  end
  for k = 1:numel(runtime.files)
    item = runtime.files(k);
    target = fullfile(destination, item.relative);
    parent = fileparts(target);
    if ~isfolder(parent)
      mkdir(parent);
    end
    if isfile(target) || isfolder(target)
      error('Nelson:nelsonc:runtimeCollision', 'Runtime destination already exists: %s.', item.relative);
    end
    [success, message] = copyfile(item.source, target);
    if ~success
      error('Nelson:nelsonc:copy', '%s: %s -> %s', message, item.source, target);
    end
    if ~strcmpi(sha256(target, '-file'), item.sha256)
      error('Nelson:nelsonc:runtimeChanged:runtimeFileChangedAfterAnalysis', 'Runtime file changed after analysis: %s.', item.source);
    end
  end
  orderFile = fullfile(destination, 'modules', 'modules.m');
  write_modules(orderFile, runtime.moduleOrder);
  inventory = runtime;
  for field = {'bootstrapFiles', 'inputs', 'startupOrder'}
    if isfield(inventory, field{1})
      inventory = rmfield(inventory, field{1});
    end
  end
  if ~isempty(inventory.libraries)
    inventory.libraries = rmfield(inventory.libraries, 'source');
  end
  inventory.files = rmfield(inventory.files, 'source');
  inventory.generated = struct('relative', 'modules/modules.m', 'sha256', sha256(orderFile, '-file'));
  write_text(fullfile(destination, 'runtime.json'), jsonencode(inventory));
end

function validate_destinations(files, directories)
  names = [{files.relative}, directories(:)'];
  for k = 1:numel(names)
    name = names{k};
    parts = strsplit(strrep(name, '\', '/'), '/');
    if isempty(name) || startsWith(name, '/') || contains(name, '\') || contains(name, ':') ...
      || any(strcmp(parts, '..')) || any(strcmp(parts, '.')) || any(cellfun('isempty', parts))
      error('Nelson:nelsonc:runtimePath', 'Invalid runtime destination path: %s.', name);
    end
  end
  keys = string({files.relative});
  directoryKeys = string(directories);
  if ispc()
    keys = lower(keys);
    directoryKeys = lower(directoryKeys);
  end
  if numel(unique(keys)) ~= numel(keys) || any(ismember(keys, directoryKeys)) ...
    || any(ismember(keys, ["runtime.json", "modules/modules.m"]))
    error('Nelson:nelsonc:runtimeCollision:conflictingRuntimeDestinationPaths', 'Conflicting runtime destination paths.');
  end
end

function write_modules(filename, modules)
  fid = fopen(filename, 'wt', 'n', 'UTF-8');
  if fid < 0
    error('Nelson:nelsonc:write:cannotCreateRuntimeStartupList', 'Cannot create runtime startup list.');
  end
  closer = onCleanup(@() fclose(fid));
  fprintf(fid, 'modules_list = {\n');
  for k = 1:numel(modules)
    fprintf(fid, '{''%s'', true};\n', modules{k});
  end
  fprintf(fid, '};\n');
  clear closer;
end

function write_text(filename, text)
  fid = fopen(filename, 'wt', 'n', 'UTF-8');
  if fid < 0
    error('Nelson:nelsonc:write:cannotCreateRuntimeInventory', 'Cannot create runtime inventory.');
  end
  closer = onCleanup(@() fclose(fid));
  fprintf(fid, '%s', text);
  clear closer;
end
