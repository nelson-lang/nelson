%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

function runtime = __nelsonc_runtime_plan__(application, gui)
  runtime = jsondecode(__nelsonc_plan__('runtime', jsonencode(application), gui));
  libraryDirectory = modulepath('nelson', 'builtin');
  pending = runtime.libraries(:);
  % Installed-runtime matching reads the interpreter's public library filename.
  pending{end + 1, 1} = fullfile(libraryDirectory, ['libnlsInterpreter', getdynlibext()]);
  % Cached gateway identities may resolve to versioned library filenames.
  % Module loaders still need the public names returned by modulepath.
  for k = 1:numel(runtime.modules)
    name = runtime.modules{k};
    if ismodule(name)
      gateway = modulepath(name, 'builtin');
      if isfile(gateway)
        pending{end + 1, 1} = gateway;
      end
    end
  end
  pending = [pending; __nelsonc_runtime_backends__(runtime, libraryDirectory)];
  if isfield(application, 'nativeRuntimeLibraries')
    for k = 1:numel(application.nativeRuntimeLibraries)
      library = application.nativeRuntimeLibraries(k);
      pending{end + 1} = library.source;
      runtime.inputs(end + 1) = struct('source', library.source, 'sha256', library.sha256);
    end
  end
  runtime.gui = gui;
  for folder = __nelsonc_graphics_assets__(runtime)
    source = fullfile(libraryDirectory, folder{1});
    if isfolder(source); source = __nelsonc_canonical_path__(source); end
    pending = [pending; native_files(source)];
  end
  [runtime.libraries, runtime.systemLibraries] = __nelsonc_native_closure__(pending, libraryDirectory);
  if ismac() && runtime.gui
    root = __nelsonc_graphics_root__(runtime, libraryDirectory);
    if ~strcmp(root, libraryDirectory)
      for folder = __nelsonc_graphics_assets__(runtime)
        source = __nelsonc_graphics_source__(root, folder{1});
        if isfolder(source); source = __nelsonc_canonical_path__(source); end
        pending = [pending; native_files(source)];
      end
      [runtime.libraries, runtime.systemLibraries] = ...
        __nelsonc_native_closure__(pending, libraryDirectory);
    end
  end
  for k = 1:numel(application.files)
    item = application.files(k);
    if ~isempty(item.module)
      runtime.inputs(end + 1) = struct('source', item.source, 'sha256', item.sha256);
    end
  end
  runtime = __nelsonc_runtime_files__(runtime, nelsonroot(), libraryDirectory);
end

function files = native_files(directory)
  files = {};
  if ~isfolder(directory)
    return;
  end
  entries = dir(directory);
  for k = 1:numel(entries)
    item = entries(k);
    if strcmp(item.name, '.') || strcmp(item.name, '..')
      continue;
    end
    filename = fullfile(directory, item.name);
    if item.isdir
      files = [files; native_files(filename)];
    elseif endsWith(item.name, '.dll') || endsWith(item.name, '.dylib') || contains(item.name, '.so')
      files{end + 1, 1} = filename;
    end
  end
end
