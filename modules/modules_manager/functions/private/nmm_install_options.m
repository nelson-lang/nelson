%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_install_options(varargin)
  if length(varargin) == 3
    report = installThree(varargin{1}, varargin{2}, varargin{3});
  elseif length(varargin) == 4
    report = installFour(varargin{1}, varargin{2}, varargin{3}, varargin{4});
  else
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function report = installThree(packageName, versionSpec, option)
  report = [];
  validateText(packageName, '#2');
  validateText(versionSpec, '#3');
  validateText(option, '#4');
  if strcmp(versionSpec, '-offline') && strcmp(option, '-dry-run')
    report = dryRunOfflinePackage(packageName, '');
  elseif strcmp(option, '-offline')
    nmm('install', nmm_cache('latest', packageName, versionSpec));
  elseif strcmp(option, '-dry-run')
    report = nmm_install_registry_dry_run(packageName, versionSpec);
  elseif strcmp(option, '-no-tests')
    installRegistryPackage(packageName, versionSpec, 'off', false);
  elseif strcmp(option, '-tests')
    installRegistryPackage(packageName, versionSpec, 'on', false);
  elseif strcmp(option, '-force')
    installRegistryPackage(packageName, versionSpec, 'auto', true);
  else
    nmm_error('InvalidOptionExpected', '%s expected.', sprintf('#%d argument: ''-offline'', ''-dry-run'', ''-tests'', ''-no-tests'' or ''-force''', 4));
  end
end
%=============================================================================
function report = installFour(packageName, versionSpec, option1, option2)
  validateText(packageName, '#2');
  validateText(versionSpec, '#3');
  validateText(option1, '#4');
  validateText(option2, '#5');
  offline = strcmp(option1, '-offline') || strcmp(option2, '-offline');
  dryRun = strcmp(option1, '-dry-run') || strcmp(option2, '-dry-run');
  if offline && dryRun
    report = dryRunOfflinePackage(packageName, versionSpec);
  else
    nmm_error('InvalidOptionOfflineDryRun', '#%d and #%d arguments: ''-offline'' and ''-dry-run'' expected.', 4, 5);
  end
end
%=============================================================================
function validateText(value, name)
  if ~ischar(value)
    nmm_error('CharactersExpectedCharactersExpected', '%s argument: characters expected.', name);
  end
end
%=============================================================================
function report = dryRunOfflinePackage(packageName, versionSpec)
  source = nmm_cache('latest', packageName, versionSpec);
  report = nmm('install', source, '-dry-run');
  report.offline = true;
  report.source = source;
end
%=============================================================================
function installRegistryPackage(packageName, versionSpec, testsMode, force)
  % testsMode: 'on' (-tests), 'off' (-no-tests) or 'auto' (environment). A
  % registry entry declaring "tests": "required" always runs them.
  plan = nmm_install_registry_dry_run(packageName, versionSpec);
  source = plan.install_plan(1).source;
  info = nmm_registry('info', packageName, versionSpec);
  if isfield(info, 'tests') && ischar(info.tests) && strcmpi(info.tests, 'required')
    testsMode = 'on';
  end
  if force && nmm_is_installed(plan.package, plan.version)
    nmm('remove', plan.package, plan.version, '-force');
  end
  isSource = isdir(source) || nmm_is_http_repository(source);
  if isSource && strcmp(testsMode, 'on')
    nmm('install', source, '-tests');
  elseif isSource && strcmp(testsMode, 'off')
    nmm('install', source, '-no-tests');
  elseif isfile(source) && force
    nmm('install', source, '-force');
  else
    nmm('install', source);
  end
end
%=============================================================================
