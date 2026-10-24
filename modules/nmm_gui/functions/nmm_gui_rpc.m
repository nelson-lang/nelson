%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function txt = nmm_gui_rpc(method, paramsJson)
  % JSON facade used by the package manager window (nmm_gui). Read-only:
  % every method wraps a nmm(...) query and returns a JSON text so the C++
  % transport stays a pure marshaller. Errors are reported in the payload
  % ({ok:false, error:...}) rather than thrown, so the window keeps working.
  params = struct();
  if nargin > 1 && ~isempty(paramsJson)
    try
      params = jsondecode(paramsJson);
    catch
      params = struct();
    end
  end
  try
    switch method
      case 'snapshot'
        out = snapshot(logicalParam(params, 'fresh'));
      case 'list'
        out = listInstalled();
      case 'search'
        out = searchRegistry(textParam(params, 'query'));
      case 'preview'
        out = withRegistryReuse(@() installPreview(textParam(params, 'name'), textParam(params, 'version')));
      case 'versions'
        out = withRegistryReuse(@() packageVersions(textParam(params, 'name')));
      case 'registry'
        out = registryStatus();
      case 'tree'
        out = withRegistryReuse(@() packageTree(textParam(params, 'name'), textParam(params, 'version'), logicalParam(params, 'installed')));
      case 'cache'
        out = cacheState();
      case 'cacheClear'
        nmm('cache', 'clear');
        out = cacheState();
      case 'cacheRemove'
        nmm('cache', 'remove', textParam(params, 'name'), textParam(params, 'version'));
        out = cacheState();
      case 'cacheVerify'
        out = cacheState();
        out.verify = nmm('cache', 'verify');
      case 'freeze'
        out = struct();
        out.manifest = freezeManifest();
      case 'doc'
        out = packageDoc(textParam(params, 'kind'), textParam(params, 'path'), textParam(params, 'repository'));
      case 'verifyInstalled'
        out = verifyInstalled(textParam(params, 'name'));
      otherwise
        error('Nelson:nmm_gui:unknownMethod', 'Unknown nmm_gui method: %s', method);
    end
    out.ok = true;
  catch e
    out = failure(e);
  end
  txt = jsonencode(out);
end
%=============================================================================
function value = textParam(params, name)
  value = '';
  if isstruct(params) && isfield(params, name)
    value = params.(name);
    if isstring(value)
      value = char(value);
    end
    if ~ischar(value)
      value = '';
    end
  end
end
%=============================================================================
function out = snapshot(fresh)
  % Everything the window shows on a refresh, with one registry download: the
  % status check fetches the registry, then the installed list and the search
  % reuse that verified copy (NELSON_NMM_REGISTRY_REUSE, see nmm_registry_fetch).
  % A user refresh (fresh) also skips the short-lived on-disk registry cache,
  % so it always downloads the current registry.
  % Each part reports its own failure, so a registry problem never hides the
  % installed packages.
  out = struct();
  if fresh
    out.registry = withEnv('NELSON_NMM_REGISTRY_CACHE_TTL', '0', @() partOf(@registryStatus));
  else
    out.registry = partOf(@registryStatus);
  end
  out.list = withRegistryReuse(@() partOf(@listInstalled));
  out.search = withRegistryReuse(@() partOf(@searchAll));
  out.arch = computer('arch');
end
%=============================================================================
function out = searchAll()
  out = searchRegistry('');
end
%=============================================================================
function out = withRegistryReuse(query)
  % Run a registry query on the copy fetched by the last refresh (see
  % nmm_registry_fetch): the detail panel then matches the list on screen and
  % a click does not download the registry again. Refresh fetches it fresh.
  out = withEnv('NELSON_NMM_REGISTRY_REUSE', '1', query);
end
%=============================================================================
function out = withEnv(name, value, query)
  % Run query with an environment variable set, restored afterwards.
  previous = getenv(name);
  setenv(name, value);
  try
    out = query();
  catch e
    setenv(name, previous);
    rethrow(e);
  end
  setenv(name, previous);
end
%=============================================================================
function out = partOf(query)
  try
    out = query();
    out.ok = true;
  catch e
    out = failure(e);
  end
end
%=============================================================================
function out = failure(e)
  % The identifier lets the window tell "no registry configured" apart from
  % a registry that exists but cannot be read.
  out = struct();
  out.ok = false;
  out.error = e.message;
  out.error_id = e.identifier;
end
%=============================================================================
function out = listInstalled()
  modules = nmm('list');
  items = {};
  if isstruct(modules)
    registered = registryEntries(modules);
    for name = fieldnames(modules)'
      data = modules.(name{1});
      item = struct();
      item.name = name{1};
      item.version = fieldOr(data, 'version', '');
      item.path = fieldOr(data, 'path', '');
      item.load = logical(fieldOr(data, 'load', false));
      item.active = logical(ismodule(name{1}));
      item.pinned = fieldOr(data, 'pinned_version', '');
      item.versions = installedVersions(data);
      item = mergeModuleJson(item, item.path);
      item = mergeDiskUsage(item);
      item = mergeRegistryState(item, registered);
      items{end + 1} = item;
    end
  end
  out = struct();
  out.installed = items;
end
%=============================================================================
function item = mergeModuleJson(item, modulePath)
  % modules.json only records path/version/load: pick up the descriptive
  % fields from the installed module.json so the Installed tab is informative.
  if ~ischar(modulePath) || isempty(modulePath)
    return
  end
  moduleJson = [modulePath, 'module.json'];
  if ~endsWith(modulePath, '/') && ~endsWith(modulePath, '\')
    moduleJson = [modulePath, '/module.json'];
  end
  if ~isfile(moduleJson)
    return
  end
  try
    info = jsondecode(moduleJson, '-file');
  catch
    return
  end
  for field = {'summary', 'description', 'keywords', 'platforms', 'nelson', 'homepage', 'license', 'dependencies', 'repository', 'issues', 'documentation'}
    if isfield(info, field{1}) && ~isfield(item, field{1})
      item.(field{1}) = info.(field{1});
    end
  end
end
%=============================================================================
function item = mergeDiskUsage(item)
  % Size on disk and install date (module.json time, else the folder's).
  item.bytes = 0;
  item.installed_at = '';
  if ~ischar(item.path) || isempty(item.path) || ~isdir(item.path)
    return
  end
  item.bytes = folderBytes(item.path);
  stamp = dir(fullfile(item.path, 'module.json'));
  if isempty(stamp)
    stamp = dir(item.path);
  end
  if ~isempty(stamp)
    item.installed_at = datestr(stamp(1).datenum, 'yyyy-mm-dd HH:MM');
  end
end
%=============================================================================
function b = folderBytes(folder)
  b = 0;
  entries = dir(folder);
  for k = 1:numel(entries)
    e = entries(k);
    if e.isdir
      if ~strcmp(e.name, '.') && ~strcmp(e.name, '..')
        b = b + folderBytes(fullfile(folder, e.name));
      end
    else
      b = b + e.bytes;
    end
  end
end
%=============================================================================
function packages = registryEntries(modules)
  % Every registry entry, yanked ones included, read once for the whole list:
  % each registry query downloads and verifies the registry again.
  packages = struct([]);
  if isempty(fieldnames(modules))
    return
  end
  try
    report = nmm('registry', 'list');
    packages = report.packages;
  catch
    packages = struct([]);
  end
end
%=============================================================================
function item = mergeRegistryState(item, packages)
  % Flag an installed version that the registry has since yanked (search no
  % longer lists it, so the window would not know otherwise).
  item.yanked = false;
  item.yank_reason = '';
  if isempty(item.version) || isempty(packages)
    return
  end
  match = strcmp({packages.name}, item.name) & strcmp({packages.version}, item.version);
  if ~any(match)
    return
  end
  info = packages(find(match, 1));
  if isfield(info, 'yanked') && ~isempty(info.yanked)
    value = info.yanked;
    if islogical(value) || isnumeric(value)
      item.yanked = logical(value(1));
    elseif ischar(value)
      item.yanked = any(strcmpi(value, {'true', '1', 'yes'}));
    end
  end
  if item.yanked && isfield(info, 'yank_reason') && ischar(info.yank_reason)
    item.yank_reason = info.yank_reason;
  end
end
%=============================================================================
function versions = installedVersions(data)
  versions = {};
  if isfield(data, 'versions') && ~isempty(data.versions)
    for k = 1:numel(data.versions)
      versions{end + 1} = fieldOr(data.versions(k), 'version', '');
    end
  elseif isfield(data, 'version')
    versions = {data.version};
  end
end
%=============================================================================
function out = searchRegistry(query)
  % The window only offers packages installable on this machine, so it opts in
  % to the platform filter: a binary package built for another architecture
  % (its "platforms" excludes computer('arch')) is not listed here, while the
  % core nmm('registry', 'search') still reports the whole registry.
  packages = nmm('registry', 'search', query, '-platform');
  out = struct();
  out.query = query;
  out.packages = packagesToCells(packages);
end
%=============================================================================
function out = packageVersions(name)
  out = struct();
  out.name = name;
  out.versions = nmm('registry', 'versions', name);
end
%=============================================================================
function out = registryStatus()
  out = nmm('registry', 'check');
end
%=============================================================================
function out = installPreview(name, version)
  % What an install or update of name (version) would do: the dry-run plan
  % (dependencies to install) and the registry entry of that version (package
  % type, license, optional size), shown before the command is queued.
  out = struct();
  if isempty(version)
    out.plan = nmm('install', name, '-dry-run');
    out.package = dropEmptyFields(nmm('registry', 'info', name));
  else
    out.plan = nmm('install', name, version, '-dry-run');
    out.package = dropEmptyFields(nmm('registry', 'info', name, version));
  end
end
%=============================================================================
function out = packageTree(name, version, installed)
  % Resolved dependency tree. An installed package uses its locked tree; a
  % registry package is resolved through an install dry-run plan.
  out = struct();
  if installed
    out.tree = nmm('tree', name);
  elseif isempty(version)
    out.tree = nmm('tree', nmm('install', name, '-dry-run'));
  else
    out.tree = nmm('tree', nmm('install', name, version, '-dry-run'));
  end
end
%=============================================================================
function out = cacheState()
  out = struct();
  out.dir = nmm('cache', 'dir');
  sz = nmm('cache', 'size');
  out.archives = sz.archives;
  out.bytes = sz.bytes;
  packages = nmm('cache', 'list');
  entries = {};
  for k = 1:numel(packages)
    e = struct();
    e.name = packages(k).name;
    e.version = packages(k).version;
    e.has_sha256 = logical(packages(k).has_sha256);
    e.bytes = fileBytes(packages(k).path);
    entries{end + 1} = e;
  end
  out.entries = entries;
end
%=============================================================================
function b = fileBytes(path)
  b = 0;
  d = dir(path);
  if ~isempty(d)
    b = d(1).bytes;
  end
end
%=============================================================================
function value = logicalParam(params, name)
  value = false;
  if isstruct(params) && isfield(params, name)
    v = params.(name);
    if islogical(v) || isnumeric(v)
      value = logical(v);
    elseif ischar(v)
      value = any(strcmpi(v, {'true', '1', 'yes'}));
    end
  end
end
%=============================================================================
function m = freezeManifest()
  m = '';
  modules = nmm('list');
  if ~isstruct(modules)
    return
  end
  names = sort(fieldnames(modules));
  lines = {};
  for k = 1:numel(names)
    ver = fieldOr(modules.(names{k}), 'version', '');
    lines{end + 1} = strtrim([names{k}, ' ', ver]);
  end
  m = strjoin(lines, char(10));
end
%=============================================================================
function out = packageDoc(kind, path, repository)
  % Readme or changelog of an installed package (its folder), else of the
  % repository of a registry package (raw file of a GitHub or GitLab project).
  out = struct();
  out.text = '';
  if ~isempty(path) && isdir(path)
    for name = docCandidates(kind)
      f = fullfile(path, name{1});
      if isfile(f)
        out.text = fileread(f);
        return
      end
    end
  end
  base = rawRepositoryBase(repository);
  if isempty(base)
    return
  end
  options = weboptions('Timeout', 10);
  candidates = docCandidates(kind);
  for name = candidates(1:2)
    try
      out.text = webread([base, name{1}], options);
      return
    catch
      % not published under this name: try the next one
    end
  end
end
%=============================================================================
function base = rawRepositoryBase(repository)
  % https://github.com/owner/repo(.git) -> raw.githubusercontent.com/owner/repo/HEAD/
  % https://gitlab.com/owner/repo(.git) -> gitlab.com/owner/repo/-/raw/HEAD/
  base = '';
  if ~ischar(repository) || isempty(repository)
    return
  end
  url = regexprep(strtrim(repository), '(\.git)?/?$', '');
  tokens = regexp(url, '^https://github\.com/([^/]+)/([^/]+)$', 'tokens', 'once');
  if ~isempty(tokens)
    base = ['https://raw.githubusercontent.com/', tokens{1}, '/', tokens{2}, '/HEAD/'];
    return
  end
  tokens = regexp(url, '^https://gitlab\.com/(.+)$', 'tokens', 'once');
  if ~isempty(tokens)
    base = ['https://gitlab.com/', tokens{1}, '/-/raw/HEAD/'];
  end
end
%=============================================================================
function names = docCandidates(kind)
  if strcmpi(kind, 'changelog')
    names = {'CHANGELOG.md', 'changelog.md', 'ChangeLog.md', 'CHANGES.md', 'changes.md', 'NEWS.md', 'CHANGELOG'};
  else
    names = {'README.md', 'readme.md', 'Readme.md', 'README.markdown', 'README.txt', 'readme.txt', 'README'};
  end
end
%=============================================================================
function out = verifyInstalled(name)
  out = struct();
  out.ok = true;
  out.issues = {};
  modules = nmm('list');
  if ~isstruct(modules) || ~isfield(modules, name)
    out.ok = false;
    out.issues = {'Module is not installed.'};
    return
  end
  data = modules.(name);
  path = fieldOr(data, 'path', '');
  ver = fieldOr(data, 'version', '');
  if isempty(path) || ~isdir(path)
    out.ok = false;
    out.issues = {'Installed path is missing.'};
    return
  end
  moduleJson = fullfile(path, 'module.json');
  if ~isfile(moduleJson)
    out.ok = false;
    out.issues = {'module.json is missing.'};
    return
  end
  try
    info = jsondecode(moduleJson, '-file');
  catch
    out.ok = false;
    out.issues = {'module.json is not readable.'};
    return
  end
  moduleVersion = docVersion(info);
  if ~isempty(moduleVersion) && ~isempty(ver) && ~strcmp(moduleVersion, ver)
    out.ok = false;
    out.issues{end + 1} = ['Version mismatch: installed ', ver, ', module.json ', moduleVersion, '.'];
  end
  dependencies = struct();
  if isfield(info, 'dependencies') && isstruct(info.dependencies)
    dependencies = info.dependencies;
  end
  for field = fieldnames(dependencies)'
    if ~isfield(modules, field{1})
      out.ok = false;
      out.issues{end + 1} = ['Dependency not installed: ', field{1}, '.'];
    end
  end
end
%=============================================================================
function v = docVersion(info)
  v = '';
  if isfield(info, 'version') && ischar(info.version)
    v = info.version;
  elseif isfield(info, 'module') && isstruct(info.module) && isfield(info.module, 'version') && ischar(info.module.version)
    v = info.module.version;
  end
end
%=============================================================================
function cells = packagesToCells(packages)
  % jsonencode of a struct array with identical fields is fine, but entries
  % read from a registry can carry heterogeneous fields: normalise to cells.
  % The registry normalisation fills a field an entry lacks with [], which
  % would reach the window as a (truthy) empty array: drop it instead.
  cells = {};
  for k = 1:numel(packages)
    cells{end + 1} = dropEmptyFields(packages(k));
  end
end
%=============================================================================
function s = dropEmptyFields(s)
  for field = fieldnames(s)'
    value = s.(field{1});
    if isa(value, 'double') && isempty(value)
      s = rmfield(s, field{1});
    end
  end
end
%=============================================================================
function value = fieldOr(data, name, fallback)
  value = fallback;
  if isstruct(data) && isfield(data, name)
    value = data.(name);
  end
end
%=============================================================================
