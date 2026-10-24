%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~isfile([modulepath('modules_manager'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
languages = {'en_US', 'fr_FR'};
for k = 1:numel(languages)
  source = fullfile(directory, languages{k}, 'xml');
  destination = fullfile(directory, languages{k}, 'html');
  mkdir(source);
  mkdir(destination);
  helpRoot = fullfile(modulepath('modules_manager'), 'help', languages{k}, 'xml');
  copyfile(fullfile(helpRoot, 'chapter.xml'), fullfile(source, 'chapter.xml'));
  copyfile(fullfile(helpRoot, 'ncc.xml'), fullfile(source, 'ncc.xml'));
  asserts.istrue(xmldocchecker(source));
  [status, message] = xmldoctohtml(source, destination, 'modules_manager', true);
  asserts.istrue(status, message);
  generated = fullfile(destination, 'ncc.html');
  asserts.istrue(isfile(generated));
  html = fileread(generated);
  required = {'ncc', 'nelsonc', 'compiler', '--explain-link', '--gui', '--cli', '.nbc', 'application.runtime', '--runtime', 'NELSONC_RUNTIME_ROOT', 'NELSON_RUNTIME_PATH', 'uicontrol', 'HandleVisibility'};
  required = [required, {'WITHOUT_COMPILER_MODULE', 'WithoutCompilerModule', 'file_archiver', 'dynamic_link', 'json', 'LD_LIBRARY_PATH', 'AT_SECURE'}];
  required = [required, {'Debian 13', 'bin/linux', 'modulepath', 'code 1', 'jsondecode', 'jsonencode'}];
  required = [required, {'LongPathsEnabled', 'MAX_PATH', '1980-01-01', '--help-file', 'helpText', 'isdeployed', 'ctfroot'}];
  required = [required, {'%#function', '%#exclude', 'exclusions', 'explicitOverride', 'referenceKind'}];
  required = [required, {'-X', '--no-auto-data', 'autoDetectDataFiles', 'ignored-data', 'dlopen'}];
  required = [required, {'-a input', 'roots/N', 'directories', 'pathEntries', 'searchRoots'}];
  required = [required, {'--no-console', 'NoConsole', 'standaloneWindowsApplication'}];
  required = [required, {'--executable-version', 'ExecutableVersion', '65535', '1.0.0.0'}];
  for item = 1:numel(required)
    asserts.istrue(contains(html, required{item}));
  end
  asserts.istrue(contains(fileread(fullfile(destination, 'index.html')), 'ncc.html'));
  asserts.isfalse(contains(html, 'nelsonc('));
  source = fullfile(directory, languages{k}, 'interpreter-xml');
  destination = fullfile(directory, languages{k}, 'interpreter-html');
  mkdir(source);
  mkdir(destination);
  helpRoot = fullfile(modulepath('interpreter'), 'help', languages{k}, 'xml');
  copyfile(fullfile(helpRoot, 'chapter.xml'), fullfile(source, 'chapter.xml'));
  copyfile(fullfile(helpRoot, 'isdeployed.xml'), fullfile(source, 'isdeployed.xml'));
  copyfile(fullfile(helpRoot, 'ctfroot.xml'), fullfile(source, 'ctfroot.xml'));
  asserts.istrue(xmldocchecker(source));
  [status, message] = xmldoctohtml(source, destination, 'interpreter', true);
  asserts.istrue(status, message);
  asserts.istrue(contains(fileread(fullfile(destination, 'isdeployed.html')), 'isdeployed'));
  asserts.istrue(contains(fileread(fullfile(destination, 'ctfroot.html')), 'ctfroot'));
end
clear cleanup;
