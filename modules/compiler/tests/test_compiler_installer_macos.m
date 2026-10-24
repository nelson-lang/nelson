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
% <--TIMEOUT 900-->
if ~ismac(); return; end
ncc('--help');
check_macos_packages();

function check_macos_packages()
  [~, token] = fileparts(tempname());
  work = fullfile(tempdir(), ['compiler mac ', token, ' ', char([233, 28450])]);
  mkdir(work); cleanup = onCleanup(@() rmdir(work, 's'));
  source = fullfile(work, 'source'); mkdir(source);
  entry = fullfile(source, 'mac_console.m');
  filewrite(entry, ['function mac_console(value); ', ...
    'if ~isdeployed() || ismodule(''compiler''); error(''Invalid deployed state.''); end; ', ...
    'fprintf(''MAC_INSTALL_OK=%s\n'', value); end']);
  build = compiler.build.standaloneApplication(entry, ...
    'OutputDir', fullfile(work, 'build'), 'EmbedArchive', false);
  installRoot = fullfile(work, 'installed');
  location = user_target_location(installRoot);
  options = compiler.package.InstallerOptions(build, ...
    'ApplicationName', 'Mac console test', 'InstallerName', 'Mac console test', ...
    'OutputDir', fullfile(work, 'package'), 'DefaultInstallationDir', location, ...
    'RuntimeDelivery', 'installer');
  compiler.package.installer(build, 'Options', options);
  package = fullfile(options.OutputDir, 'Mac console test.pkg');
  asserts.istrue(isfile(package));
  listing = invoke({'/usr/sbin/pkgutil', '--payload-files', package}, true);
  asserts.istrue(contains(listing, './mac_console.runtime/bin/macOS/libnlsInterpreter.dylib'));
  asserts.istrue(contains(listing, './mac_console.nca'));
  compiler.runtime.customInstaller('Mac runtime test', build, ...
    'RuntimeDelivery', 'installer', 'OutputDir', fullfile(work, 'runtime package'));
  runtimePackage = fullfile(work, 'runtime package', 'Mac runtime test.pkg');
  asserts.istrue(isfile(runtimePackage));
  runtimeListing = invoke({'/usr/sbin/pkgutil', '--payload-files', runtimePackage}, true);
  asserts.istrue(contains(runtimeListing, './payload/bin/macOS/libnlsInterpreter.dylib'));
  if isempty(location); clear cleanup; return; end
  invoke({'/usr/sbin/installer', '-pkg', package, '-target', 'CurrentUserHomeDirectory'}, true);
  rmdir(source, 's'); rmdir(build.Options.OutputDir, 's');
  application = fullfile(installRoot, 'mac_console');
  output = clean_run(application, work, 'first');
  asserts.istrue(contains(output, 'MAC_INSTALL_OK=first'), output);
  asserts.isfalse(contains(output, fullfile(nelsonroot(), 'bin', 'macOS', 'libnls')));
  readme = fullfile(installRoot, 'readme.txt');
  originalReadme = fileread(readme);
  filewrite(readme, [originalReadme, 'USER CHANGE']);
  invoke({'/usr/sbin/installer', '-pkg', package, '-target', 'CurrentUserHomeDirectory'}, false);
  filewrite(readme, originalReadme);
  filewrite(fullfile(installRoot, 'user data.txt'), 'PRESERVE');
  invoke({'/usr/sbin/installer', '-pkg', package, '-target', 'CurrentUserHomeDirectory'}, true);
  output = clean_run(application, work, 'second');
  asserts.istrue(contains(output, 'MAC_INSTALL_OK=second'), output);
  invoke({fullfile(installRoot, '.nelson-install', 'uninstall')}, true);
  asserts.isfalse(isfile(application));
  asserts.istrue(isfile(fullfile(installRoot, 'user data.txt')));
  clear cleanup;
end

function location = user_target_location(destination)
  home = fullpath(getenv('HOME')); destination = fullpath(destination);
  prefix = [home, filesep];
  if ~startsWith(destination, prefix)
    location = '';
    return;
  end
  location = ['/', strrep(destination(numel(prefix) + 1:end), '\', '/')];
end

function output = clean_run(application, work, argument)
  home = fullfile(work, 'home');
  if ~isfolder(home); mkdir(home); end
  output = invoke({'/usr/bin/env', '-i', ['HOME=', home], ...
    'PATH=/usr/bin:/bin:/usr/sbin:/sbin', 'NELSONC_RUNTIME_ROOT=', ...
    'NELSON_RUNTIME_PATH=', 'DYLD_LIBRARY_PATH=', 'DYLD_FRAMEWORK_PATH=', ...
    'DYLD_PRINT_LIBRARIES=1', application, argument}, true);
end

function output = invoke(parts, success)
  quote = char(39);
  for k = 1:numel(parts)
    parts{k} = [quote, strrep(parts{k}, quote, char([39 34 39 34 39])), quote];
  end
  [status, output] = system([strjoin(parts, ' '), ' 2>&1'], 600);
  asserts.isequal(status == 0, success, output);
end
