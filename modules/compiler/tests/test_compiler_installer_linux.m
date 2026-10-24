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
% <--TIMEOUT 600-->
if ispc() || ismac(); return; end
ncc('--help');
check_installer();

function check_installer()
  work = [tempname(), ' installer '' ', char([28450, 23383])];
  mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  entry = fullfile(work, 'linux_install_app.m');
  filewrite(entry, ['function linux_install_app(output); disp(''LINUX_INSTALLED_OK''); ', ...
    '[status, maps] = system(''cat /proc/$PPID/maps''); ', ...
    'if status ~= 0; error(''Cannot inspect runtime libraries.''); end; filewrite(output, maps); end']);
  built = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(work, 'build'), ...
    'EmbedArchive', false, 'RuntimeLogFile', 'application.log');
  extra = fullfile(work, 'resources'); mkdir(extra);
  mkdir(fullfile(extra, 'empty'));
  filewrite(fullfile(extra, 'A.txt'), 'UPPER');
  filewrite(fullfile(extra, 'a.txt'), 'lower');
  filewrite(fullfile(extra, '$(touch INJECTION_MARKER)'), 'LITERAL_NAME');
  filewrite(fullfile(extra, [repmat('x', 1, 130), '.txt']), 'LONG_NAME');
  counterpart = fullfile(work, 'LINUX_INSTALL_APP'); filewrite(counterpart, 'CASE_COUNTERPART');
  options = compiler.package.InstallerOptions(built, 'RuntimeDelivery', 'none', ...
    'ApplicationName', 'Linux test', 'InstallerName', 'Linux test', ...
    'OutputDir', fullfile(work, 'package'), 'AdditionalFiles', {extra, counterpart});
  options.AuthorCompany = 'Publisher '' quoted $value';
  options.DefaultInstallationDir = fullfile(work, 'default destination');
  compiler.package.installer(built, 'Options', options);
  installer = fullfile(options.OutputDir, 'Linux test.install');
  asserts.istrue(isfile(installer));
  invoke('/usr/bin/touch', {'-t', '200102030405', fullfile(extra, 'A.txt')}, true);
  options.OutputDir = fullfile(work, 'repeat');
  compiler.package.installer(built.Files, fullfile(built.Options.OutputDir, 'buildresult.json'), 'Options', options);
  asserts.isequal(sha256(installer, '-file'), sha256(fullfile(options.OutputDir, 'Linux test.install'), '-file'));
  rejected(@() compiler.package.installer(built, 'Options', options));
  options.PackageType = 'zip'; options.OutputDir = fullfile(work, 'zip');
  rejected(@() compiler.package.installer(built, 'Options', options));
  asserts.isfalse(isfolder(options.OutputDir));
  options.PackageType = 'auto'; options.RuntimeDelivery = 'installer';
  options.OutputDir = fullfile(work, 'bundled');
  compiler.package.installer(built, 'Options', options);
  bundled = fullfile(options.OutputDir, 'Linux test.install');
  rmfile(entry);
  check_rejections(installer, work);
  check_removal_boundaries(installer, work);
  destination = fullfile(work, 'installed');
  log = fullfile(work, 'install.log');
  invoke(installer, {'-agreeToLicense', 'yes', '-applicationFolder', destination, '-outputFile', log}, true);
  asserts.istrue(contains(fileread(log), 'Installed Linux test'));
  asserts.isequal(fileread(fullfile(destination, 'resources', 'A.txt')), 'UPPER');
  asserts.isequal(fileread(fullfile(destination, 'resources', 'a.txt')), 'lower');
  asserts.isequal(fileread(fullfile(destination, 'resources', '$(touch INJECTION_MARKER)')), 'LITERAL_NAME');
  asserts.isfalse(isfile(fullfile(destination, 'INJECTION_MARKER')));
  asserts.isequal(fileread(fullfile(destination, 'LINUX_INSTALL_APP')), 'CASE_COUNTERPART');
  asserts.istrue(isfolder(fullfile(destination, 'resources', 'empty')));
  link = fullfile(destination, 'Launch Linux test');
  output = invoke('/usr/bin/readlink', {link}, true);
  asserts.isequal(strtrim(output), 'linux_install_app');
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  invoke(link, {fullfile(work, 'installed.maps')}, true);
  invoke(installer, {'-agreeToLicense', 'yes', '-applicationFolder', destination}, true);
  appLog = fileread(fullfile(destination, 'application.log'));
  filewrite(fullfile(destination, 'user-data.txt'), 'KEEP');
  filewrite(fullfile(destination, 'resources', 'A.txt'), 'MODIFIED');
  invoke(installer, {'-agreeToLicense', 'yes', '-applicationFolder', destination}, false);
  invoke(fullfile(destination, '.nelson-install', 'uninstall.sh'), {}, true);
  asserts.isfalse(isfile(fullfile(destination, 'linux_install_app')));
  asserts.isequal(fileread(fullfile(destination, 'resources', 'A.txt')), 'MODIFIED');
  asserts.isequal(fileread(fullfile(destination, 'user-data.txt')), 'KEEP');
  asserts.isequal(fileread(fullfile(destination, 'application.log')), appLog);
  invoke(fullfile(destination, '.nelson-install', 'uninstall.sh'), {}, true);
  setenv('NELSONC_RUNTIME_ROOT', '');
  destination = fullfile(work, 'private runtime');
  control = fullfile(work, 'install.txt');
  filewrite(control, [char(65279), 'agreeToLicense=yes', char([13 10]), ...
    'applicationFolder=', destination, char([13 10])]);
  invoke(bundled, {'-inputFile', control}, true);
  maps = fullfile(work, 'bundled.maps');
  invoke(fullfile(destination, 'linux_install_app'), {maps}, true);
  loaded = fileread(maps);
  asserts.istrue(contains(loaded, [destination, '/linux_install_app.runtime/bin/linux/libnlsEngine']));
  asserts.isfalse(contains(loaded, modulepath('nelson', 'builtin')));
  invoke(fullfile(destination, '.nelson-install', 'uninstall.sh'), {}, true);
  asserts.isfalse(isfolder(fullfile(destination, 'linux_install_app.runtime')));
  asserts.istrue(isfile(fullfile(destination, 'application.log')));
  clear restore cleanup;
end

function check_removal_boundaries(installer, work)
  destination = fullfile(work, 'boundary');
  outside = fullfile(work, 'outside'); mkdir(outside);
  untouched = fullfile(outside, 'A.txt'); filewrite(untouched, 'UPPER');
  before = sha256(untouched, '-file');
  alias = fullfile(work, 'alias');
  invoke('/usr/bin/ln', {'-s', outside, alias}, true);
  invoke(installer, {'-agreeToLicense', 'yes', '-applicationFolder', alias}, false);
  dangling = fullfile(work, 'dangling');
  absent = fullfile(outside, 'absent');
  invoke('/usr/bin/ln', {'-s', absent, dangling}, true);
  for suffix = {'', '/', '/./'}
    invoke(installer, {'-agreeToLicense', 'yes', '-applicationFolder', [dangling, suffix{1}]}, false);
    asserts.isfalse(isfolder(absent));
  end
  invoke(installer, {'-agreeToLicense', 'yes', '-destinationFolder', destination}, true);
  manifest = fullfile(destination, '.nelson-install', 'manifest.tsv');
  original = fileread(manifest);
  application = fullfile(destination, 'linux_install_app');
  appHash = sha256(application, '-file');
  filewrite(manifest, [original, 'invalid']);
  uninstall = fullfile(destination, '.nelson-install', 'uninstall.sh');
  invoke(uninstall, {}, false);
  asserts.isequal(sha256(application, '-file'), appHash);
  filewrite(manifest, original);
  invoke('/usr/bin/mv', {fullfile(destination, 'resources'), fullfile(work, 'saved-resources')}, true);
  invoke('/usr/bin/ln', {'-s', outside, fullfile(destination, 'resources')}, true);
  invoke(uninstall, {}, true);
  asserts.isequal(sha256(untouched, '-file'), before);
  asserts.isfalse(isfile(application));
  asserts.istrue(isfile(manifest));
end

function check_rejections(installer, work)
  destination = fullfile(work, 'refused');
  invoke(installer, {}, false);
  invoke(installer, {'-applicationFolder', destination, '-agreeToLicense', 'yes'}, false);
  invoke(installer, {'-agreeToLicense', 'yes', '-applicationFolder', destination, '-destinationFolder', destination}, false);
  invoke(installer, {'-agreeToLicense', 'yes', '-runtimeFolder', destination}, false);
  control = fullfile(work, 'invalid.txt');
  for contents = {['agreeToLicense=yes', newline(), 'unknown=value'], ...
    ['agreeToLicense=yes', newline(), 'applicationFolder=', destination, newline(), ...
    'applicationFolder=', destination], ['agreeToLicense=yes', char(0), newline()]}
    filewrite(control, contents{1});
    invoke(installer, {'-inputFile', control}, false);
  end
  asserts.isfalse(isfolder(destination));
  existingLog = fullfile(work, 'existing.log'); filewrite(existingLog, 'KEEP_LOG');
  invoke(installer, {'-agreeToLicense', 'yes', '-applicationFolder', destination, '-outputFile', existingLog}, false);
  asserts.isequal(fileread(existingLog), 'KEEP_LOG');
  asserts.isfalse(isfolder(destination));
  mkdir(destination); filewrite(fullfile(destination, 'keep.txt'), 'KEEP');
  invoke(installer, {'-agreeToLicense', 'yes', '-applicationFolder', destination}, false);
  asserts.isequal(fileread(fullfile(destination, 'keep.txt')), 'KEEP');
  corrupted = fullfile(work, 'corrupted.install'); copyfile(installer, corrupted);
  fid = fopen(corrupted, 'ab'); fwrite(fid, uint8(1), 'uint8'); fclose(fid);
  invoke(corrupted, {'-agreeToLicense', 'yes', '-applicationFolder', fullfile(work, 'corrupt')}, false);
  asserts.isfalse(isfolder(fullfile(work, 'corrupt')));
end

function output = invoke(command, arguments, success)
  quote = char(39);
  values = [{command}, arguments];
  for k = 1:numel(values)
    values{k} = [quote, strrep(values{k}, quote, char([39 34 39 34 39])), quote];
  end
  [status, output] = system([strjoin(values, ' '), ' 2>&1'], 180);
  asserts.isequal(status == 0, success, output);
end

function rejected(callback)
  caught = false;
  try; callback(); catch; caught = true; end
  asserts.istrue(caught);
end
