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
previousDocOpenPath = getenv('NELSON_DOC_OPEN_PATH');
previousDeployLangs = getenv('NELSON_HELP_DEPLOY_LANGUAGES');
previousHelpModules = getenv('NELSON_HELP_MODULES');
previousDocRoot = docroot();
destination = [tempdir(), 'test_doc_', createGUID()];
cleanupEnv = onCleanup(@() setenv('NELSON_HELP_CACHE_DIR', previousHelpCache)); %#ok<NLS0004>
cleanupDocDryRun = onCleanup(@() setenv('NELSON_DOC_DRY_RUN', previousDocDryRun)); %#ok<NLS0004>
cleanupDocOpenPath = onCleanup(@() setenv('NELSON_DOC_OPEN_PATH', previousDocOpenPath)); %#ok<NLS0004>
cleanupDeployLangs = onCleanup(@() setenv('NELSON_HELP_DEPLOY_LANGUAGES', previousDeployLangs)); %#ok<NLS0004>
cleanupHelpModules = onCleanup(@() setenv('NELSON_HELP_MODULES', previousHelpModules)); %#ok<NLS0004>
cleanupDocRoot = onCleanup(@() docroot(previousDocRoot)); %#ok<NLS0004>
cleanupCommand = ['if isdir(''', destination, '''), rmdir(''', destination, ''', ''s''); end'];
cleanupDir = onCleanup(@() evalc(cleanupCommand)); %#ok<NLS0004>
% Deploy a single language and module to keep this sequential test fast.
setenv('NELSON_HELP_DEPLOY_LANGUAGES', 'en_US');
setenv('NELSON_HELP_MODULES', 'help_tools');
setenv('NELSON_HELP_CACHE_DIR', destination);
deployhelp('install', false);
docroot('.');
setenv('NELSON_DOC_DRY_RUN', 'TRUE');
launcherPath = [destination, '/dry_run_launcher.html'];
setenv('NELSON_DOC_OPEN_PATH', launcherPath);
%=============================================================================
url = strtrim(evalc('doc;'));
asserts.istrue(contains(url, '/index.html'));
url = strtrim(evalc('doc(''help_tools'');'));
asserts.istrue(startsWith(url, 'file://'));
asserts.istrue(contains(url, '/index.html#open='));
asserts.istrue(contains(url, 'help_tools%2Findex.html'));
url = strtrim(evalc('doc doc'));
asserts.istrue(startsWith(url, 'file://'));
asserts.istrue(contains(url, '/index.html#open='));
asserts.istrue(contains(url, 'help_tools%2Fdoc.html'));
% Resolving a dry-run URL must not write a browser launcher.
asserts.isfalse(isfile(launcherPath));
%=============================================================================
