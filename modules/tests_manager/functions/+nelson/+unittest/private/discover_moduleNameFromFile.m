%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [moduleName, moduleRoot] = discover_moduleNameFromFile(filename)
  moduleName = '';
  moduleRoot = '';
  if ~ischar(filename) || isempty(filename) || ~isfile(filename)
    return
  end
  directory = fileparts(fullpath(filename));
  while ~isempty(directory)
    % A module's startup file establishes its boundary. Directory names such
    % as a package version or .nmm-tmp are not module identities.
    if isfile(fullfile(directory, 'etc', 'startup.m'))
      moduleName = moduleNameAtRoot(directory);
      if ~isempty(moduleName)
        moduleRoot = normalizedModulePath(directory);
      end
      return
    end
    parent = fileparts(directory);
    if strcmp(parent, directory)
      return
    end
    directory = parent;
  end
end
%=============================================================================
function moduleName = moduleNameAtRoot(directory)
  moduleName = '';
  manifest = fullfile(directory, 'module.json');
  if isfile(manifest)
    try
      info = jsondecode(manifest, '-file');
    catch
      return
    end
    if isstruct(info) && isscalar(info) && isfield(info, 'module')
      value = info.module;
      if isstring(value) && isscalar(value)
        value = char(value);
      end
      if ischar(value) && isrow(value) && ~isempty(regexp(value, '^[A-Za-z][A-Za-z0-9_]*$', 'once'))
        moduleName = value;
      end
    end
    return
  end
  % Shipped modules have no package manifest. Their exact installation root
  % identifies optional modules even when they are not loaded in this process.
  [parent, name] = fileparts(directory);
  if sameModulePath(parent, fullfile(nelsonroot(), 'modules'))
    moduleName = name;
    return
  end
  % A manually registered legacy module may also lack a package manifest.
  [names, roots] = getmodules();
  for k = 1:numel(names)
    if sameModulePath(directory, roots{k})
      moduleName = names{k};
      return
    end
  end
end
%=============================================================================
function tf = sameModulePath(first, second)
  first = normalizedModulePath(first);
  second = normalizedModulePath(second);
  if ispc()
    tf = strcmpi(first, second);
  else
    tf = strcmp(first, second);
  end
end
%=============================================================================
function value = normalizedModulePath(value)
  value = replace(fullpath(value), '\', '/');
  while length(value) > 1 && value(end) == '/'
    value(end) = [];
  end
end
