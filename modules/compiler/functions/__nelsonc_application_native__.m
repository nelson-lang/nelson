%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function plan = __nelsonc_application_native__(plan)
  plan.nativeLibraries = struct([]);
  plan.nativeRuntimeLibraries = struct([]);
  pending = {};
  for k = 1:numel(plan.files)
    item = plan.files(k);
    if strcmp(item.representation, 'native') && isempty(item.module)
      pending{end + 1, 1} = item.source;
    end
  end
  if isempty(pending)
    return;
  end
  if ~ispc()
    error('Nelson:nelsonc:nativePlatform', 'Application native dependency deployment is currently supported on Windows only.');
  end
  libraryDirectory = strrep(fullpath(modulepath('nelson', 'builtin')), '\', '/');
  [libraries, plan.nativeSystemLibraries] = __nelsonc_native_closure__(pending, libraryDirectory);
  for k = 1:numel(libraries)
    library = libraries(k);
    runtimeFile = fullfile(libraryDirectory, library.filename);
    runtimeOwned = startsWith(lower(library.source), [lower(libraryDirectory), '/']);
    if isfile(runtimeFile) && ~strcmpi(sha256(runtimeFile, '-file'), library.sha256)
      error('Nelson:nelsonc:nativeConflict', 'Application library conflicts with the runtime: %s.', library.filename);
    end
    if runtimeOwned
      plan.nativeRuntimeLibraries = append_library(plan.nativeRuntimeLibraries, library);
      continue;
    end
    plan.nativeLibraries = append_library(plan.nativeLibraries, library);
    sources = {plan.files.source};
    index = find(strcmpi(sources, library.source), 1);
    if ~isempty(index)
      if ~strcmpi(plan.files(index).sha256, library.sha256)
        error('Nelson:nelsonc:sourceChanged', 'Application input changed after analysis: %s.', library.source);
      end
      continue;
    end
    root = fileparts(library.source);
    for j = 1:numel(plan.searchRoots)
      candidate = plan.searchRoots{j};
      if startsWith(lower(library.source), [lower(candidate), '/'])
        root = candidate;
        break;
      end
    end
    if ~any(strcmpi(plan.searchRoots, root))
      plan.searchRoots{end + 1} = root;
    end
    details = dir(library.source);
    item = struct('source', library.source, 'sha256', library.sha256, 'bytes', details.bytes, ...
      'reason', 'native-dependency', 'module', '', 'representation', 'native', ...
      'root', root, 'relative', library.source(length(root) + 2:end));
    plan.files(end + 1) = item;
    if numel(plan.files) > 10000
      error('Nelson:nelsonc:nativeLimit:applicationFileCountLimitExceeded', 'Application file count limit exceeded.');
    end
  end
end

function libraries = append_library(libraries, library)
  if isempty(libraries)
    libraries = library;
  else
    libraries(end + 1) = library;
  end
end
