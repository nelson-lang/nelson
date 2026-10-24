%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function versions = nmm_installed(varargin)
  if length(varargin) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  moduleName = varargin{1};
  if ~ischar(moduleName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  modules = nmm_list();
  versions = {};
  if ~isfield(modules, moduleName)
    return
  end
  data = modules.(moduleName);
  records = installedVersions(data);
  for k = 1:length(records)
    versions{end + 1} = records(k).version;
  end
end
%=============================================================================
function versions = installedVersions(moduleData)
  versions = struct([]);
  if isfield(moduleData, 'versions') && ~isempty(moduleData.versions)
    versions = moduleData.versions;
    return
  end
  if isfield(moduleData, 'path') && isfield(moduleData, 'version')
    versions = moduleData;
    if isfield(versions, 'versions')
      versions = rmfield(versions, 'versions');
    end
    if isfield(versions, 'pinned_version')
      versions = rmfield(versions, 'pinned_version');
    end
  end
end
%=============================================================================
