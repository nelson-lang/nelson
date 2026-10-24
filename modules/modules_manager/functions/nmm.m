%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = nmm(varargin)
  if nargin == 0
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  [args, quiet] = nmm_quiet_option(varargin);
  localList = ischar(args{1}) && strcmp(args{1}, 'list');
  if requiresWebtools(args) && ~ismodule('webtools')
    nmm_error('RequiredModuleMissing', 'nmm required webtools module.');
  end
  if ~localList && ~ismodule('file_archiver')
    nmm_error('RequiredModuleMissingNmmRequiredFile', 'nmm required file_archiver module.');
  end
  if ~ismodule('json')
    nmm_error('RequiredModuleMissingNmmRequiredJson', 'nmm required json module.');
  end

  if quiet
    evalc('varargout = nmm_dispatch(args, nargout);');
  else
    varargout = nmm_dispatch(args, nargout);
  end
end
%=============================================================================
function required = requiresWebtools(args)
  % Local package operations use only the filesystem and archive modules. Keep
  % the network dependency for registry and repository operations, but do not
  % prevent an embedded runtime from installing an imported local package.
  command = args{1};
  required = false;
  if ~ischar(command)
    required = true;
    return
  end
  if strcmp(command, 'publish')
    required = true;
    return
  end
  registryCommands = {'registry', 'search', 'info', 'versions', 'outdated', 'update'};
  if any(strcmp(command, registryCommands))
    required = ~usesLocalRegistry();
    return
  end
  if strcmp(command, 'install')
    if length(args) >= 2 && ischar(args{2}) && (isfile(args{2}) || isdir(args{2}))
      return
    end
    if length(args) >= 2 && ischar(args{2}) && endsWith(args{2}, '.nmz')
      % a local archive path that does not exist is a user error, not a
      % registry request (typical in the browser when the upload went elsewhere)
      nmm_error('ArchiveNotFound', 'Package archive not found: %s', args{2});
    end
    dryRun = any(cellfun(@(arg) ischar(arg) && strcmp(arg, '-dry-run'), args));
    required = ~(dryRun && usesLocalRegistry());
  end
end
%=============================================================================
function tf = usesLocalRegistry()
  source = nmm_registry_source();
  tf = ~nmm_is_remote_registry(source);
end
%=============================================================================
function outputs = nmm_dispatch(args, requestedOutputs)
  outputs = {};
  firstParameter = args{1};
  if ~isSupportedFirstParameter(firstParameter)
    nmm_error('UnknownCommand', '#1 value or type not managed.');
  end
  try
    switch firstParameter
      case 'list'
        if length(args) > 2
          nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
        end
        outputs{1} = nmm_list(args{2:end});
      case 'commands'
        if length(args) > 2
          nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
        end
        outputs{1} = nmm_commands('commands', args{2:end});
      case 'help'
        outputs{1} = nmm_commands('help', args{2:end});
      case 'load'
        if requestedOutputs ~= 0
          nmm_error('WrongNumberOfOutputArguments', 'Wrong number of output arguments.');
        end
        nmm_load(args{2:end});
      case 'verify'
        outputs{1} = nmm_verify(args{2:end});
      case 'lock'
        outputs{1} = nmm_lock(args{2:end});
      case 'config'
        outputs{1} = nmm_config(args{2:end});
      case 'pin'
        outputs{1} = nmm_pin(args{2:end});
      case 'unpin'
        outputs{1} = nmm_unpin(args{2:end});
      case 'why'
        outputs{1} = nmm_why(args{2:end});
      case 'tree'
        outputs{1} = nmm_tree(args{2:end});
      case 'deps'
        outputs{1} = nmm_deps(args{2:end});
      case 'rdeps'
        outputs{1} = nmm_rdeps(args{2:end});
      case 'status'
        outputs{1} = nmm_status(args{2:end});
      case 'explain'
        outputs{1} = nmm_explain(args{2:end});
      case 'graph'
        outputs{1} = nmm_graph(args{2:end});
      case 'installed'
        outputs{1} = nmm_installed(args{2:end});
      case 'latest'
        outputs{1} = nmm_latest(args{2:end});
      case 'resolve'
        outputs{1} = nmm_resolve(args{2:end});
      case 'satisfies'
        outputs{1} = nmm_satisfies(args{2:end});
      case 'autoload'
        outputs{1} = nmm_autoload(args{2:end});
      case 'uninstall'
        if requestedOutputs == 0
          nmm_uninstall(args{2:end});
        else
          outputs{1} = nmm_uninstall(args{2:end});
        end
      case 'remove'
        if requestedOutputs == 0
          nmm_uninstall(args{2:end});
        else
          outputs{1} = nmm_uninstall(args{2:end});
        end
      case 'install'
        if requestedOutputs == 0
          nmm_install(args{2:end});
        else
          outputs{1} = nmm_install(args{2:end});
        end
      case 'package'
        outputs{1} = nmm_package(args{2:end});
      case 'pack'
        outputs{1} = nmm_pack(args{2:end});
      case 'publish'
        outputs{1} = nmm_publish(args{2:end});
      case 'init'
        outputs{1} = nmm_init(args{2:end});
      case 'validate'
        outputs{1} = nmm_validate(args{2:end});
      case 'audit'
        outputs{1} = nmm_audit(args{2:end});
      case 'doctor'
        outputs{1} = nmm_doctor(args{2:end});
      case 'repair'
        outputs{1} = nmm_repair(args{2:end});
      case 'orphans'
        outputs{1} = nmm_orphans(args{2:end});
      case 'autoremove'
        outputs{1} = nmm_autoremove(args{2:end});
      case 'registry'
        outputs{1} = nmm_registry(args{2:end});
      case 'search'
        outputs{1} = nmm_registry('search', args{2:end});
      case 'info'
        outputs{1} = nmm_registry('info', args{2:end});
      case 'versions'
        outputs{1} = nmm_registry('versions', args{2:end});
      case 'outdated'
        outputs{1} = nmm_outdated(args{2:end});
      case 'update'
        outputs{1} = nmm_update(args{2:end});
      case 'cache'
        outputs{1} = nmm_cache(args{2:end});
      case 'i18n'
        outputs{1} = nmm_i18n(args{2:end});
      otherwise
        nmm_error('UnknownCommand', '#1 value or type not managed.')
    end
  catch
    e = lasterror();
    error(e);
  end
end
%=============================================================================
function r = isSupportedFirstParameter(param)
  if ischar(param)
    supported = ["list", "commands", "help", "load", "verify", "lock", "config", "pin", "unpin", "why", "tree", "deps", "rdeps", "status", "explain", "graph", "installed", "latest", "resolve", "satisfies", "autoload", "install", "uninstall", "remove", "package", "pack", "publish", "init", "validate", "audit", "doctor", "repair", "orphans", "autoremove", "registry", "search", "info", "versions", "outdated", "update", "cache", "i18n"];
    r = any(strcmp(supported, param));
  else
    r = false;
  end
end
%=============================================================================
