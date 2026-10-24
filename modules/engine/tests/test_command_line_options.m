%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
binpath = modulepath('nelson', 'bin');
nelson_cli = [binpath, '/nelson-cli'];
%=============================================================================
[status, output] = run_nelson_cli(nelson_cli, ' --quiet --no-user-startup --no-ipc --execute="quit"');
asserts.isequal(status, 0);
%=============================================================================
if ~ispc()
  badModulesRoot = [tempdir(), 'test_command_line_options_bad_modules_', createGUID(), '/'];
  previousExternalModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');
  previousExternalModulesPathLiteral = ['''', strrep(previousExternalModulesPath, '''', ''''''), ''''];
  badModulesRootLiteral = ['''', strrep(badModulesRoot, '''', ''''''), ''''];
  cleanupCommand = ['setenv(''NELSON_EXTERNAL_MODULES_PATH'', ', previousExternalModulesPathLiteral, ');', newline, ...
    'if isdir(', badModulesRootLiteral, '), rmdir(', badModulesRootLiteral, ', ''s''), end'];
  mkdir(badModulesRoot);
  cleanupBadModules = onCleanup(@() evalc(cleanupCommand)); %#ok<NASGU,NLS0004>
  filewrite([badModulesRoot, 'modules.json'], '[]');
  setenv('NELSON_EXTERNAL_MODULES_PATH', badModulesRoot);
  quotedCommand = ['"', replace(nelson_cli, '"', '\"'), '"'];
  [status, output] = system(['printf "exit\n" | ', quotedCommand, ' --quiet --no-ipc'], 30);
  asserts.isequal(status, 0);
  asserts.isfalse(contains(output, 'Impossible to read modules.json'));
  modulesInfo.missing_module.path = [badModulesRoot, 'missing_module/1.0.0/'];
  modulesInfo.missing_module.load = true;
  modulesInfo.missing_module.version = '1.0.0';
  filewrite([badModulesRoot, 'modules.json'], jsonprettyprint(jsonencode(modulesInfo)));
  [status, output] = system(['printf "1\nexit\n" | ', quotedCommand, ' --quiet --no-ipc'], 30);
  asserts.isequal(status, 0);
  asserts.istrue(contains(output, 'Impossible to load module: missing_module'));
  asserts.istrue(contains(output, 'ans'));
  asserts.istrue(contains(output, '1'));
  evalc(cleanupCommand); %#ok<NLS0004>
end
%=============================================================================
