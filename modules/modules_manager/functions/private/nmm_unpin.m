%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_unpin(varargin)
  if length(varargin) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  module_name = varargin{1};
  if ~ischar(module_name)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  modules = nmm_list();
  if ~isfield(modules, module_name)
    nmm_error('ModuleNameExpectedModuleNameExpected', 'module name expected.');
  end
  module_record = getfield(modules, module_name);
  wasPinned = isfield(module_record, 'pinned_version');
  versions = installedVersions(module_record);
  selected = selectInstalledVersion(versions, latestInstalledVersion(versions));
  selected.versions = versions;
  modules.(module_name) = selected;
  filewrite([usermodulesdir(), 'modules.json'], jsonprettyprint(jsonencode(modules)));
  if wasPinned || ~strcmp(module_record.version, selected.version)
    if ismodule(module_name)
      removemodule(module_name);
    end
    nmm_load(module_name, selected.version);
  end
  report = struct();
  report.module = module_name;
  report.version = selected.version;
  report.unpinned = wasPinned;
end
%=============================================================================
function versions = installedVersions(module_data)
  versions = struct([]);
  if isfield(module_data, 'versions') && ~isempty(module_data.versions)
    versions = module_data.versions;
    return
  end
  if isfield(module_data, 'path') && isfield(module_data, 'version')
    versions = module_data;
    if isfield(versions, 'versions')
      versions = rmfield(versions, 'versions');
    end
    if isfield(versions, 'pinned_version')
      versions = rmfield(versions, 'pinned_version');
    end
  end
end
%=============================================================================
function version = latestInstalledVersion(versions)
  version = versions(1).version;
  for k = 2:length(versions)
    if semver(versions(k).version, version) == 1
      version = versions(k).version;
    end
  end
end
%=============================================================================
function selected = selectInstalledVersion(versions, version)
  selected = struct([]);
  for k = 1:length(versions)
    if strcmp(versions(k).version, version)
      selected = versions(k);
      return
    end
  end
end
%=============================================================================
