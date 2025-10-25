%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nmm_install(varargin)
  switch length(varargin)
    case 1
      nmm_install_one_rhs(varargin{1});
    case 2
      nmm_install_two_rhs(varargin{1}, varargin{2});
    otherwise
      error(_('Wrong number of input arguments.'));
    end
  end
  %=============================================================================
  % nmm('install', module_name)
  % nmm('install', local_module_directory)
  % nmm('install', http_git_repository)
function nmm_install_one_rhs(param1)
  if isfile(param1)
    nmm_install_file(param1);
  else
    if isdir(param1)
      nmm_install_directory(param1);
    else
      [res, url, tag_branch] = nmm_is_http_repository(param1);
      if res
        nmm_install_repository(url, tag_branch);
      else
        error(_('An valid git url or local directory expected.'))
      end
    end
  end
end
%=============================================================================
% nmm('install', module_name, module_version)
function nmm_install_two_rhs(module_name, module_version)
  error('Currently not managed.')
end
%=============================================================================
function nmm_install_file(filename)
  if ~endsWith(filename, '.nmz')
    error(_('filename extension .nmz expected.'));
  end
  destinationTempPath = [tempdir(), createGUID(), '/'];
  unzip(filename, destinationTempPath);
  lockFile = [destinationTempPath, 'module-lock.json'];
  if ~isfile(lockFile)
    error(_('module-lock.json is missing.'));
  end
  info = jsondecode(lockFile, '-file');
  if nmm_is_installed(info.module)
    warning(sprintf(_('%s already installed.'), info.module));
    [r, msg] = rmdir(destinationTempPath, 's');
    return
  end
  for m = fieldnames(info.dependencies)'
    if ~nmm_is_installed(m{1})
      [r, msg] = rmdir(destinationTempPath, 's');
      error(sprintf(_('%s not installed.'), m{1}));
    end
  end
  isSupported = strcmp(info.platform, 'all') || strcmp(info.platform, computer('arch'));
  if ~isSupported
    [r, msg] = rmdir(destinationTempPath, 's');
    error(_('platform not supported.'));
  end
  destination_dir = copyModule(destinationTempPath, info.module, info.version);
  saveInstalledModule(info.module, destination_dir, info.version);
  nmm_load(info.module);
  [r, msg] = rmdir(destinationTempPath, 's');
end
%=============================================================================
function nmm_install_directory(directory)
  if ~endsWith(directory, '/')
    directory = [directory, '/'];
  end
  if ~is_local_module(directory)
    error(_('Valid module repository expected.'));
  end
  module_description = nmm_read_module_json(directory);
  if nmm_is_installed(module_description.module)
    warning(sprintf(_('%s already installed.'), module_description.module));
    return
  end
  [r, msg] = nmm_is_supported_platform(module_description.platforms);
  if ~r
    error(msg);
  end
  if (module_description.builtin && ~havecompiler())
    installCompiler = false;
    if ispc()
      [installCompiler, message] = configuremsvc();
    end
    if installCompiler
      warning(_('Visual studio detected and configured to be used with Nelson.'));
    else
      error(sprintf(_('An C/C++ compiler is required by %s'), module_description.module));
    end
  end
  destination_dir = copyModule(directory, module_description.module, module_description.version);
  
  try
    run([destination_dir, 'builder.m']);
    createModuleLockJson(destination_dir);
    saveInstalledModule(module_description.module, destination_dir, module_description.version);
    nmm_load(module_description.module);
  catch
    e = lasterror();
    error(e);
  end
end
%=============================================================================
function nmm_install_repository(url, tag_branch)
  destinationTempPath = [tempdir(), createGUID(), '/'];
  repo('export', url, tag_branch, destinationTempPath);
  module_description = nmm_read_module_json(destinationTempPath);
  if nmm_is_installed(module_description.module)
    [r, msg] = rmdir(destinationTempPath, 's');
    warning(sprintf(_('%s already installed.'), module_description.module));
    return
  end
  [r, msg] = nmm_is_supported_platform(module_description.platforms);
  if ~r
    [r, msg] = rmdir(destinationTempPath, 's');
    error(msg);
  end
  if (module_description.builtin && ~havecompiler())
    installCompiler = false;
    if ispc()
      [installCompiler, message] = configuremsvc();
    end
    if installCompiler
      warning(_('Visual studio detected and configured to be used with Nelson.'));
    else
      [r, msg] = rmdir(destinationTempPath, 's');
      error(sprintf(_('An C/C++ compiler is required by %s'), module_description.module));
    end
  end
  destination_dir = copyModule(destinationTempPath, module_description.module, module_description.version);
  [r, msg] = rmdir(destinationTempPath, 's');
  try
    run([destination_dir, 'builder.m']);
    createModuleLockJson(destination_dir);
    saveInstalledModule(module_description.module, destination_dir, module_description.version);
    nmm_load(module_description.module);
  catch
    e = lasterror();
    [r, msg] = rmdir(destinationTempPath, 's');
    error(e);
  end
end
%=============================================================================
function r = is_local_module(param)
  r  = false;
  if isdir(param)
    path = fullpath(param);
    if ~endsWith(path, '/')
      path = [path, '/'];
    end
    r = isfile([path, 'module.json']) && isfile([path, 'builder.m']);
  end
end
%=============================================================================
function destination_dir = copyModule(source, module_name, module_version)
  p = usermodulesdir();
  destination_dir = [p, module_name, '/', module_version, '/'];
  if ~isdir(destination_dir)
    mkdir(destination_dir);
  end
  copyfile(source, destination_dir, 'f');
  deployhelp('add', module_name, destination_dir);
end
%=============================================================================
function saveInstalledModule(module_name, module_path, version)
  element.path = module_path;
  element.load = true;
  element.version = version;
  modules = nmm_list();
  modules.(module_name) = element;
  p = usermodulesdir();
  modules_json_path = [p, 'modules.json'];
  txt = jsonprettyprint(jsonencode(modules));
  filewrite(modules_json_path, txt);
end
%=============================================================================
function createModuleLockJson(module_path)
  info = nmm_read_module_json(module_path);
  moduleLockJson = [module_path, '/module-lock.json'];
  st = struct();
  st.module = info.module;
  st.version = info.version;
  st.lock = 1;
  if ~info.builtin
    st.platform = 'all';
  else
    st.platform = computer('arch');
  end
  st.builtin = info.builtin;
  modules = readModulesJson();
  dependencies = struct();
  for m = fieldnames(info.dependencies)'
    dependencies.(m{1}) = dependencies.(m{1}).version;
  end
  st.dependencies = dependencies;
  content = jsonprettyprint(jsonencode(st));
  filewrite(moduleLockJson, content);
end
%=============================================================================
function data = readModulesJson()
  p = usermodulesdir();
  modules_json_path = [p, 'modules.json'];
  if isfile(modules_json_path)
    data = jsondecode(modules_json_path, '-file');
  else
    data = struct();
  end
end
%=============================================================================
