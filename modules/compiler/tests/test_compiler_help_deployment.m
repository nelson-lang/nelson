%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% Deploying help requires the prebuilt help archives; skip when they are not
% available (for example a build configured without help generation).
skip_testsuite(isempty(dir([nelsonroot(), '/modules/main/help/*.nhz'])), 'Main help archive is missing')
%=============================================================================
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
variables = {'NELSON_HELP_CACHE_DIR', 'NELSON_HELP_DEPLOY_LANGUAGES', 'NELSON_HELP_MODULES'};
values = cellfun(@getenv, variables, 'UniformOutput', false);
restoreEnvironment = onCleanup(@() restore_environment(variables, values));
setenv('NELSON_HELP_CACHE_DIR', directory);
setenv('NELSON_HELP_DEPLOY_LANGUAGES', 'en_US,fr_FR');
setenv('NELSON_HELP_MODULES', 'compiler,modules_manager');
asserts.istrue(deployhelp('install', false));
asserts.istrue(deployhelp('refresh'));
for language = {'en_US', 'fr_FR'}
  root = fullfile(directory, language{1});
  index = fileread(fullfile(root, 'index.json'));
  for name = {'nelson.compiler.BuildOptions', 'nelson.compiler.BuildResult', ...
    'nelson.compiler.build', 'nelson.compiler.analyze', ...
    'compiler_standalone_tutorial', 'compiler_embedded_data_tutorial', 'compiler_build_tutorial', ...
    'compiler.build.standaloneApplication', 'compiler.build.standaloneWindowsApplication', ...
    'compiler.build.StandaloneApplicationOptions', 'compiler.build.Results', 'compiler.runtime.Dependencies', ...
    'compiler.package.installer', 'compiler.package.InstallerOptions', 'compiler.runtime.customInstaller', ...
    'compiler_installer_tutorial', 'compiler_linux_installer_tutorial', ...
    'compiler_macos_installer_tutorial', ...
    'compiler_runtime_tutorial', 'compiler_linux_runtime_tutorial', 'compiler_project_tutorial'}
    filename = [name{1}, '.html'];
    asserts.istrue(isfile(fullfile(root, 'compiler', filename)), filename);
    asserts.istrue(contains(index, filename), filename);
  end
  for name = {'deploytool', 'standaloneApplicationCompiler', 'ncc'}
    filename = [name{1}, '.html'];
    asserts.istrue(isfile(fullfile(root, 'modules_manager', filename)), filename);
    asserts.istrue(contains(index, filename), filename);
  end
  page = fileread(fullfile(root, 'modules_manager', 'ncc.html'));
  for text = {'XDG_CONFIG_HOME', 'XDG_CONFIG_DIRS', 'NELSON_RUNTIME_ROOT_1', ...
    'nelson/runtimes/architecture/fingerprint.root', '32768'}
    asserts.istrue(contains(page, text{1}), text{1});
  end
  page = fileread(fullfile(root, 'compiler', 'compiler.package.installer.html'));
  for text = {'-runtimeFolder', '-applicationFolder', '-destinationFolder', ...
    'XDG_CONFIG_HOME', '.nelson-runtime/uninstall', 'desktopShortcut', ...
    'startMenuShortcut', 'nelson_installer_*.log', '/ALLUSERS'}
    asserts.istrue(contains(page, text{1}), text{1});
  end
  page = fileread(fullfile(root, 'compiler', 'compiler.runtime.customInstaller.html'));
  for text = {'-agreeToLicense yes', '-destinationFolder', '-outputFile', '-inputfile', ...
    'nelson_installer_*.log', '/CURRENTUSER', '/ALLUSERS', '/PORTABLE=1'}
    asserts.istrue(contains(page, text{1}), text{1});
  end
end
clear restoreEnvironment;
clear cleanup;

function restore_environment(names, values)
  for k = 1:numel(names)
    setenv(names{k}, values{k});
  end
end
