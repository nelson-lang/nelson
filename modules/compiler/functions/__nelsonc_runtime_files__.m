%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function runtime = __nelsonc_runtime_files__(runtime, sourceRoot, libraryDirectory)
  for notice = {'gpl-3.0.md', 'lgpl-3.0.md', 'THIRDPARTY.md', 'TRADEMARKS.md'}
    if ~isfile(fullfile(sourceRoot, notice{1}))
      error('Nelson:nelsonc:notice', 'Missing runtime redistribution notice: %s.', notice{1});
    end
  end
  if ispc()
    architecture = computer('arch');
    if strcmp(architecture, 'win64')
      platform = 'x64';
    elseif strcmp(architecture, 'woa64')
      platform = 'ARM64';
    else
      error('Nelson:nelsonc:nativePlatform:applicationPackagingRequires64Bit', 'Application packaging requires a 64-bit Windows runtime.');
    end
  elseif ismac()
    platform = 'macOS';
  else
    platform = 'linux';
  end
  binaryRoot = ['bin/', platform];
  files = {};
  folders = {binaryRoot; 'modules'};
  for k = 1:numel(runtime.libraries)
    item = runtime.libraries(k);
    files{end + 1, 1} = struct('source', fullpath(item.source), ...
      'relative', [binaryRoot, '/', item.filename], 'sha256', item.sha256);
  end
  if ismac()
    [frameworkFiles, frameworkFolders] = mac_frameworks(runtime.libraries, binaryRoot);
    files = [files; frameworkFiles];
    folders = [folders; frameworkFolders];
  end
  trees = {'etc', 'resources', 'locale'};
  for k = 1:numel(trees)
    source = fullfile(sourceRoot, trees{k});
    if isfolder(source)
      [more, directories] = collect_tree(source, trees{k}, {});
      files = [files; more];
      folders = [folders; directories];
    end
  end
  graphicsRoot = __nelsonc_graphics_root__(runtime, libraryDirectory);
  for name = __nelsonc_graphics_assets__(runtime)
    source = __nelsonc_graphics_source__(graphicsRoot, name{1});
    if isfolder(source)
      source = __nelsonc_canonical_path__(source);
      [more, directories] = collect_tree(source, [binaryRoot, '/', name{1}], {});
      files = [files; more];
      folders = [folders; directories];
    end
  end
  orderFile = fullfile(sourceRoot, 'modules', 'modules.m');
  digest = sha256(orderFile, '-file');
  modules_list = read_module_order(orderFile);
  if ~strcmpi(sha256(orderFile, '-file'), digest)
    error('Nelson:nelsonc:runtimeChanged:runtimeModuleOrderChanged', 'Runtime module order changed during analysis.');
  end
  runtime.inputs(end + 1) = struct('source', fullpath(orderFile), 'sha256', digest);
  selected = {};
  runtime.startupOrder = cell(1, numel(modules_list));
  for k = 1:numel(modules_list)
    item = modules_list{k};
    runtime.startupOrder{k} = item{1};
    if any(strcmp(runtime.modules, item{1}))
      selected{end + 1} = item{1};
    end
  end
  if numel(selected) ~= numel(runtime.modules) || numel(unique(selected)) ~= numel(selected)
    error('Nelson:nelsonc:moduleOrder', 'Missing or duplicate runtime module startup ordering.');
  end
  runtime.moduleOrder = selected;
  % Module-root development trees are not deployed runtime resources.
  excluded = {'src', 'builtin', 'no-src', 'tests', 'help', 'examples', 'benchmarks', ...
    'node_modules', 'temp', 'test-results', 'CMakeFiles', 'CMakeLists.txt', 'Makefile', ...
    'cmake_install.cmake', 'CTestTestfile.cmake', 'install_manifest.txt'};
  for k = 1:numel(selected)
    source = fullfile(sourceRoot, 'modules', selected{k});
    relative = ['modules/', selected{k}];
    folders{end + 1, 1} = relative;
    contents = dir(source);
    for j = 1:numel(contents)
      name = contents(j).name;
      if strcmp(name, '.') || strcmp(name, '..') || any(strcmp(name, excluded))
        continue;
      end
      [more, directories] = collect_tree(fullfile(source, name), [relative, '/', name], {});
      files = [files; more];
      folders = [folders; directories];
    end
  end
  for name = {'COPYING', 'COPYING.LESSER', 'LICENSE', 'LICENSE.md', 'gpl-3.0.md', ...
    'lgpl-3.0.md', 'THIRDPARTY.md', 'TRADEMARKS.md'}
    source = fullfile(sourceRoot, name{1});
    if isfile(source)
      files{end + 1, 1} = file_record(source, name{1});
    end
  end
  notices = dir(fullfile(libraryDirectory, '*license*'));
  for k = 1:numel(notices)
    item = notices(k);
    if ~item.isdir
      files{end + 1, 1} = file_record(fullfile(item.folder, item.name), [binaryRoot, '/', item.name]);
    end
  end
  if numel(files) > 100000
    error('Nelson:nelsonc:runtimeLimit', 'Runtime file count limit exceeded.');
  end
  runtime.files = order_files(files);
  runtime.directories = unique(folders);
  for k = 1:numel(runtime.inputs)
    item = runtime.inputs(k);
    if ~strcmpi(sha256(item.source, '-file'), item.sha256)
      error('Nelson:nelsonc:runtimeChanged', 'Runtime source changed after analysis: %s.', item.source);
    end
  end
end

function [files, folders] = mac_frameworks(libraries, binaryRoot)
  files = {}; folders = {}; added = {};
  for k = 1:numel(libraries)
    source = strrep(libraries(k).source, '\', '/');
    marker = '.framework/'; position = strfind(source, marker);
    if isempty(position); continue; end
    boundary = position(1) + length('.framework') - 1;
    framework = source(1:boundary);
    [~, stem, extension] = fileparts(framework);
    leaf = [stem, extension];
    if any(strcmp(added, leaf)); continue; end
    added{end + 1} = leaf;
    suffix = source(boundary + 2:end);
    relative = [binaryRoot, '/Frameworks/', leaf, '/', suffix];
    files{end + 1, 1} = file_record(source, relative);
    resources = fullfile(fileparts(source), 'Resources');
    if isfolder(resources)
      target = [binaryRoot, '/Frameworks/', leaf, '/', ...
        strrep(fullfile(fileparts(suffix), 'Resources'), '\', '/')];
      [more, directories] = collect_tree(resources, target, {});
      files = [files; more]; folders = [folders; directories];
    end
  end
end

function modules_list = read_module_order(filename)
  run(filename, false);
end

function item = file_record(source, relative)
  item = struct('source', fullpath(source), 'relative', relative, 'sha256', sha256(source, '-file'));
end

function [files, folders] = collect_tree(source, relative, ancestors)
  files = {};
  folders = {};
  if ~isfolder(source)
    files{1, 1} = file_record(source, relative);
    return;
  end
  canonical = fullpath(source);
  if ispc()
    canonical = lower(canonical);
  end
  if any(strcmp(ancestors, canonical)) || numel(ancestors) >= 64
    error('Nelson:nelsonc:runtimeCycle', 'Cyclic or excessively deep runtime directory: %s.', source);
  end
  ancestors{end + 1} = canonical;
  folders{1, 1} = relative;
  contents = dir(source);
  for k = 1:numel(contents)
    name = contents(k).name;
    if strcmp(name, '.') || strcmp(name, '..') || is_build_artifact(name)
      continue;
    end
    [more, directories] = collect_tree(fullfile(source, name), [relative, '/', name], ancestors);
    files = [files; more];
    folders = [folders; directories];
    if numel(files) + numel(folders) > 100000
      error('Nelson:nelsonc:runtimeLimit', 'Runtime file count limit exceeded.');
    end
  end
end

function excluded = is_build_artifact(name)
  excluded = any(strcmp(name, {'CMakeFiles', 'CMakeLists.txt', 'Makefile', ...
    'cmake_install.cmake', 'CTestTestfile.cmake', 'install_manifest.txt'}));
end

function files = order_files(records)
  % Build the structure array once instead of repeatedly concatenating records.
  sources = cell(1, numel(records));
  relatives = cell(1, numel(records));
  digests = cell(1, numel(records));
  for k = 1:numel(records)
    item = records{k};
    sources{k} = item.source;
    relatives{k} = item.relative;
    digests{k} = item.sha256;
  end
  files = struct('source', sources, 'relative', relatives, 'sha256', digests);
  keys = {files.relative};
  if ispc()
    keys = cellstr(lower(string(keys)));
  end
  [keys, order] = sort(keys);
  files = files(order);
  keep = true(1, numel(files));
  for k = 2:numel(files)
    if strcmp(keys{k - 1}, keys{k})
      left = files(k - 1);
      right = files(k);
      if ~strcmp(left.source, right.source) || ~strcmpi(left.sha256, right.sha256)
        error('Nelson:nelsonc:runtimeCollision:runtimeInputsShareDestination', 'Runtime inputs share a destination: %s.', right.relative);
      end
      keep(k) = false;
    end
  end
  files = files(keep);
end
