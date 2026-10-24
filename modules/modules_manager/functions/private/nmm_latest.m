%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function packageInfo = nmm_latest(varargin)
  if length(varargin) < 1 || length(varargin) > 2
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  packageName = varargin{1};
  versionSpec = '';
  if length(varargin) == 2
    versionSpec = varargin{2};
  end
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if ~ischar(versionSpec)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  versions = nmm_registry('versions', packageName);
  packageInfo = struct([]);
  for k = 1:length(versions)
    if ~isempty(versionSpec) && ~nmm_satisfies(versions{k}, versionSpec)
      continue
    end
    candidate = nmm_registry('info', packageName, versions{k});
    if packageIsCompatible(candidate)
      packageInfo = latestPackage(packageInfo, candidate);
    end
  end
  if isempty(packageInfo)
    nmm_error('NoCompatiblePackageFound', 'No compatible package found: %s', packageName);
  end
end
%=============================================================================
function tf = packageIsCompatible(packageInfo)
  tf = true;
  if isfield(packageInfo, 'platforms')
    [tf, ~] = nmm_is_supported_platform(packageInfo.platforms);
    if ~tf
      return
    end
  end
  if isfield(packageInfo, 'nelson')
    tf = nmm_satisfies(nelsonVersionString(), packageInfo.nelson);
  end
end
%=============================================================================
function selected = latestPackage(selected, candidate)
  if isempty(selected) || semver(candidate.version, selected.version) == 1
    selected = candidate;
  end
end
%=============================================================================
function s = nelsonVersionString()
  v = version('-number');
  s = sprintf('%d.%d.%d', v(1), v(2), v(3));
end
%=============================================================================
