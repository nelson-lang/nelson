%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

function [libraries, systemLibraries] = __nelsonc_native_closure__(pending, libraryDirectory)
  libraries = struct([]);
  systemLibraries = {};
  keys = {};
  inherited = cell(numel(pending), 1);
  engine = jsondecode(__library_dependencies__(fullfile(libraryDirectory, ['libnlsEngine', getdynlibext()])));
  k = 1;
  while k <= numel(pending)
    source = strrep(fullpath(pending{k}), '\', '/');
    k = k + 1;
    [~, stem, extension] = fileparts(source);
    filename = [stem, extension];
    key = filename;
    if strcmp(engine.format, 'PE')
      key = lower(key);
    end
    previous = find(strcmp(keys, key), 1);
    if ~isempty(previous)
      if ~same_path(libraries(previous).source, source) ...
        && ~strcmpi(libraries(previous).sha256, sha256(source, '-file'))
        error('Nelson:nelsonc:nativeConflict:conflictingNativeLibraries', 'Conflicting native libraries: %s.', filename);
      end
      continue;
    end
    digest = sha256(source, '-file');
    info = jsondecode(__library_dependencies__(source));
    if ~strcmpi(sha256(source, '-file'), digest)
      error('Nelson:nelsonc:runtimeChanged:nativeLibraryChangedDuringAnalysis', 'Native library changed during analysis: %s.', source);
    end
    if ~strcmp(info.architecture, engine.architecture) || ~strcmp(info.format, engine.format)
      error('Nelson:nelsonc:architecture', 'Incompatible native library: %s.', source);
    end
    searchPaths = {};
    childRpaths = {};
    if strcmp(info.format, 'ELF')
      [searchPaths, childRpaths] = __nelsonc_elf_search_paths__(info, source, inherited{k - 1});
    end
    keys{end + 1} = key;
    item = struct('source', source, 'filename', filename, 'needed', {info.needed}, 'delayed', {info.delayed}, 'sha256', digest);
    if isempty(libraries)
      libraries = item;
    else
      libraries(end + 1) = item;
    end
    dependencies = [info.needed(:); info.delayed(:)];
    for j = 1:numel(dependencies)
      [resolved, system] = resolve_dependency(dependencies{j}, source, info, libraryDirectory, searchPaths);
      if system
        systemLibraries{end + 1} = dependencies{j};
      else
        pending{end + 1} = resolved;
        inherited{end + 1} = childRpaths;
      end
    end
    if numel(libraries) > 2048
      error('Nelson:nelsonc:nativeLimit:nativeLibraryCountLimitExceeded', 'Native library count limit exceeded.');
    end
  end
  systemLibraries = unique(systemLibraries);
end

function [resolved, system] = resolve_dependency(name, parent, info, libraryDirectory, searchPaths)
  resolved = '';
  system = false;
  if strcmp(info.format, 'PE')
    if any(name == '/') || any(name == '\') || any(name == ':')
      error('Nelson:nelsonc:nativePath', 'Invalid PE dependency name.');
    end
    if __nelsonc_is_system_library__(name, '')
      system = true;
      return;
    end
  elseif strcmp(info.format, 'Mach-O')
    if startsWith(name, '/usr/lib/') || startsWith(name, '/System/Library/')
      system = true;
      return;
    end
  elseif any(strcmp(name, {'libc.so.6', 'libm.so.6', 'libdl.so.2', 'libpthread.so.0', 'librt.so.1'})) ...
    || startsWith(name, 'ld-linux')
    system = true;
    return;
  end
  parentDirectory = fileparts(parent);
  if strcmp(info.format, 'ELF')
    name = __nelsonc_elf_expand_origin__(name, parentDirectory);
    if contains(name, '/')
      if isfile(name)
        resolved = name;
        return;
      end
      error('Nelson:nelsonc:nativeMissing', 'Missing native dependency %s required by %s.', name, parent);
    end
  end
  [~, stem, extension] = fileparts(name);
  leaf = [stem, extension];
  if strcmp(info.format, 'ELF')
    candidates = {};
    for k = 1:numel(searchPaths)
      candidates{end + 1} = fullfile(searchPaths{k}, name);
    end
    candidates{end + 1} = fullfile(libraryDirectory, name);
  else
    candidates = {fullfile(parentDirectory, name), fullfile(libraryDirectory, name), fullfile(libraryDirectory, leaf)};
    for k = 1:numel(info.searchPaths)
      directories = strsplit(info.searchPaths{k}, ':');
      for j = 1:numel(directories)
        folder = expand_origin(directories{j}, parentDirectory, libraryDirectory);
        localName = strrep(name, '@rpath/', '');
        candidates{end + 1} = fullfile(folder, localName);
      end
    end
  end
  if ~strcmp(info.format, 'ELF') && (startsWith(name, '/') || startsWith(name, '@loader_path/') || startsWith(name, '@executable_path/'))
    candidates{end + 1} = expand_origin(name, parentDirectory, libraryDirectory);
  end
  for k = 1:numel(candidates)
    if isfile(candidates{k})
      if strcmp(info.format, 'PE') && __nelsonc_is_system_library__(name, candidates{k})
        system = true;
      else
        resolved = candidates{k};
      end
      return;
    end
  end
  if ispc() && strcmp(info.format, 'PE')
    candidate = fullfile(getenv('SystemRoot'), 'System32', name);
    if isfile(candidate)
      if __nelsonc_is_system_library__(name, candidate)
        system = true;
      else
        resolved = candidate;
      end
      return;
    end
  elseif ~ispc() && ~ismac() && strcmp(info.format, 'ELF')
    triplet = [info.architecture, '-linux-gnu'];
    for folder = {'/lib', '/lib64', '/usr/lib', '/usr/lib64', ['/lib/', triplet], ['/usr/lib/', triplet]}
      candidate = fullfile(folder{1}, name);
      if isfile(candidate)
        resolved = candidate;
        return;
      end
    end
  end
  error('Nelson:nelsonc:nativeMissing', 'Missing native dependency %s required by %s.', name, parent);
end

function value = expand_origin(value, parent, binaries)
  value = strrep(value, '$ORIGIN', parent);
  value = strrep(value, '${ORIGIN}', parent);
  value = strrep(value, '@loader_path', parent);
  value = strrep(value, '@executable_path', binaries);
end

function same = same_path(left, right)
  left = strrep(left, '\', '/');
  right = strrep(right, '\', '/');
  if ispc()
    same = strcmpi(left, right);
  else
    same = strcmp(left, right);
  end
end
