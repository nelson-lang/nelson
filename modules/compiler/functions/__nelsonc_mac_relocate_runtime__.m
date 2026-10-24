%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function runtime = __nelsonc_mac_relocate_runtime__(runtime, root)
  binary = fullfile(root, 'bin', 'macOS');
  candidates = {};
  for k = 1:numel(runtime.libraries)
    candidates{end + 1} = ['bin/macOS/', runtime.libraries(k).filename];
  end
  for k = 1:numel(runtime.files)
    name = runtime.files(k).relative;
    if (startsWith(name, 'bin/macOS/plugins/') && endsWith(name, '.dylib')) ...
      || is_framework_binary(name)
      candidates{end + 1} = name;
    end
  end
  candidates = unique(candidates);
  for k = 1:numel(candidates)
    relative = candidates{k};
    target = fullfile(root, relative);
    if ~isfile(target); continue; end
    info = jsondecode(__library_dependencies__(target));
    preserveEngine = any(strcmp(leaf_name(relative), ...
      {'libnlsInterpreter.dylib', 'libnlsInterpreter.2.dylib'}));
    changed = false;
    if isfield(info, 'identity') && startsWith(info.identity, '/')
      identity = ['@rpath/', leaf_name(relative)];
      if contains(info.identity, '.framework/')
        identity = framework_reference(info.identity);
      end
      invoke({'/usr/bin/install_name_tool', '-id', identity, target});
      changed = true;
    end
    dependencies = [info.needed(:); info.delayed(:)];
    for j = 1:numel(dependencies)
      dependency = dependencies{j};
      if startsWith(dependency, '/usr/lib/') || startsWith(dependency, '/System/Library/')
        continue;
      end
      if contains(dependency, '.framework/')
        reference = framework_reference(dependency);
        frameworkFile = strrep(reference, '@rpath/', '');
        if ~isfile(fullfile(binary, 'Frameworks', frameworkFile))
          error('Nelson:nelsonc:nativeMissing:missingRelocatableMacOSFrameworkRequired', ...
            'Missing relocatable macOS framework %s required by %s.', dependency, relative);
        end
        if ~strcmp(dependency, reference)
          invoke({'/usr/bin/install_name_tool', '-change', dependency, reference, target});
          changed = true;
        end
        continue;
      end
      if ~relocation_required(dependency); continue; end
      [~, stem, extension] = fileparts(dependency);
      leaf = [stem, extension];
      if ~isfile(fullfile(binary, leaf))
        error('Nelson:nelsonc:nativeMissing:missingRelocatableMacOSDependencyRequired', ...
          'Missing relocatable macOS dependency %s required by %s.', dependency, relative);
      end
      reference = loader_reference(relative, leaf);
      invoke({'/usr/bin/install_name_tool', '-change', dependency, reference, target});
      changed = true;
    end
    if isfield(info, 'searchPaths') && ~preserveEngine
      for j = 1:numel(info.searchPaths)
        searchPath = info.searchPaths{j};
        if startsWith(searchPath, '/') && ~startsWith(searchPath, '/usr/lib/') ...
          && ~startsWith(searchPath, '/System/Library/')
          invoke({'/usr/bin/install_name_tool', '-delete_rpath', searchPath, target});
          changed = true;
        end
      end
    end
    if changed
      invoke({'/usr/bin/codesign', '--force', '--sign', '-', '--timestamp=none', target});
    end
  end
  runtime = refresh_inventory(runtime, root, candidates);
end

function yes = is_framework_binary(relative)
  parts = strsplit(strrep(relative, '\', '/'), '/');
  index = find(endsWith(parts, '.framework'), 1);
  yes = ~isempty(index) ...
    && strcmp(parts{end}, parts{index}(1:end - numel('.framework')));
end

function name = leaf_name(relative)
  [~, stem, extension] = fileparts(relative);
  name = [stem, extension];
end

function required = relocation_required(name)
  system = startsWith(name, '/usr/lib/') || startsWith(name, '/System/Library/');
  required = ~system && startsWith(name, '/');
end

function reference = framework_reference(name)
  marker = strfind(name, '.framework/');
  prefix = name(1:marker(1) - 1);
  slash = strfind(prefix, '/');
  if isempty(slash); start = 1; else; start = slash(end) + 1; end
  reference = ['@rpath/', name(start:end)];
end

function reference = loader_reference(relative, leaf)
  folder = strrep(fileparts(relative), '\', '/');
  parts = strsplit(folder, '/');
  levels = max(0, numel(parts) - 2);
  reference = ['@loader_path/', repmat('../', 1, levels), leaf];
end

function runtime = refresh_inventory(runtime, root, candidates)
  document = jsondecode(fileread(fullfile(root, 'runtime.json')));
  for k = 1:numel(candidates)
    relative = candidates{k};
    target = fullfile(root, relative);
    if ~isfile(target); continue; end
    digest = sha256(target, '-file');
    index = find(strcmp({runtime.files.relative}, relative));
    if ~isempty(index); runtime.files(index).sha256 = digest; end
    index = find(strcmp({document.files.relative}, relative));
    if ~isempty(index); document.files(index).sha256 = digest; end
  end
  for k = 1:numel(runtime.libraries)
    target = fullfile(root, 'bin', 'macOS', runtime.libraries(k).filename);
    digest = sha256(target, '-file');
    runtime.libraries(k).sha256 = digest;
    index = find(strcmp({document.libraries.filename}, runtime.libraries(k).filename));
    if ~isempty(index); document.libraries(index).sha256 = digest; end
  end
  filewrite(fullfile(root, 'runtime.json'), jsonencode(document));
end

function invoke(parts)
  for k = 1:numel(parts); parts{k} = shell_argument(parts{k}); end
  [status, output] = system(strjoin(parts, ' '));
  if status ~= 0
    error('Nelson:nelsonc:installerTool:cannotRelocateMacOSRuntime', 'Cannot relocate macOS runtime: %s', output);
  end
end

function value = shell_argument(value)
  quote = char(39); value = [quote, strrep(value, quote, char([39 34 39 34 39])), quote];
end
