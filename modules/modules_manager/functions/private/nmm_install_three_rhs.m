%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_install_three_rhs(module_name, module_version, option)
  report = [];
  if ~ischar(module_name)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  if ~ischar(module_version)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 3);
  end
  if ~ischar(option)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 4);
  end
  if strcmp(option, '-offline')
    installOfflinePackage(module_name, module_version);
    return
  end
  if strcmp(option, '-dry-run')
    report = nmm_install_registry_dry_run(module_name, module_version);
    return
  end
  nmm_error('InvalidOptionExpected', '%s expected.', sprintf('#%d argument: ''-offline'' or ''-dry-run''', 4));
end
%=============================================================================
function installOfflinePackage(module_name, versionSpec)
  source = nmm_cache('latest', module_name, versionSpec);
  nmm_install(source);
end
%=============================================================================
