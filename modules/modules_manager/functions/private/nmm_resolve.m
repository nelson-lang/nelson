%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_resolve(varargin)
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
  packageInfo = nmm_latest(packageName, versionSpec);
  report = struct();
  report.package = packageInfo.name;
  report.version = packageInfo.version;
  report.requested = versionSpec;
  report.package_info = packageInfo;
end
%=============================================================================
