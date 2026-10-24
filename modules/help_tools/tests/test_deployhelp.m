%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--SEQUENTIAL TEST REQUIRED-->
% <--TIMEOUT 300-->
%=============================================================================
main_help_archives = dir([nelsonroot(), '/modules/main/help/*.nhz']);
skip_testsuite(isempty(main_help_archives), 'Main help archive is missing')
%=============================================================================
previousHelpCache = getenv('NELSON_HELP_CACHE_DIR');
previousDocDryRun = getenv('NELSON_DOC_DRY_RUN');
previousDeployLangs = getenv('NELSON_HELP_DEPLOY_LANGUAGES');
previousHelpModules = getenv('NELSON_HELP_MODULES');
previousDocRoot = docroot();
destination = [tempdir(), 'test_deployhelp_', createGUID()];
cleanupEnv = onCleanup(@() setenv('NELSON_HELP_CACHE_DIR', previousHelpCache)); %#ok<NLS0004>
cleanupDocDryRun = onCleanup(@() setenv('NELSON_DOC_DRY_RUN', previousDocDryRun)); %#ok<NLS0004>
cleanupDeployLangs = onCleanup(@() setenv('NELSON_HELP_DEPLOY_LANGUAGES', previousDeployLangs)); %#ok<NLS0004>
cleanupHelpModules = onCleanup(@() setenv('NELSON_HELP_MODULES', previousHelpModules)); %#ok<NLS0004>
cleanupDocRoot = onCleanup(@() docroot(previousDocRoot)); %#ok<NLS0004>
cleanupCommand = ['if isdir(''', destination, '''), rmdir(''', destination, ''', ''s''); end'];
cleanupDir = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>
% Deploy a single language and module to keep this sequential test fast; the
% per-module extraction path is exercised in full for en_US.
setenv('NELSON_HELP_DEPLOY_LANGUAGES', 'en_US');
setenv('NELSON_HELP_MODULES', 'help_tools');
setenv('NELSON_HELP_CACHE_DIR', destination);
%=============================================================================
asserts.isfalse(deployhelp('status'));
%=============================================================================
status = deployhelp('install', false);
asserts.istrue(status);
asserts.istrue(deployhelp('status'));
asserts.istrue(isfile([destination, '/en_US/index.html']));
asserts.istrue(isfile([destination, '/en_US/index.json']));
viewer = fileread([destination, '/en_US/index.html']);
asserts.istrue(contains(viewer, 'readLaunchParams'));
asserts.istrue(contains(viewer, 'window.location.hash'));
status = deployhelp('add', 'help_tools', modulepath('help_tools'));
asserts.istrue(status);
toc = fileread([destination, '/en_US/toc.html']);
asserts.isequal(length(strfind(toc, './help_tools/help.html')), 1);
helpPage = [destination, '/en_US/help_tools/help.html'];
originalHelp = fileread(helpPage);
filewrite(helpPage, 'stale help page');
rmfile([destination, '/en_US/.deployhelp/help_tools.stamp']);
status = deployhelp('refresh');
asserts.istrue(status);
asserts.isequal(fileread(helpPage), originalHelp);
docroot('.');
setenv('NELSON_DOC_DRY_RUN', 'TRUE');
docUrl = strtrim(evalc('doc(''doc'');'));
asserts.istrue(startsWith(docUrl, 'file://'));
asserts.istrue(contains(docUrl, '/index.html#open='));
asserts.istrue(contains(docUrl, 'help_tools%2Fdoc.html'));
asserts.isfalse(contains(docUrl, '/index.html?open='));
%=============================================================================
output = evalc('deployhelp(''install'', true);');
asserts.istrue(contains(output, 'Extracting help files:'));
asserts.istrue(contains(output, '100 %'));
asserts.isfalse(contains(output, 'e+'));
asserts.isfalse(contains(output, 'e-'));
%=============================================================================
