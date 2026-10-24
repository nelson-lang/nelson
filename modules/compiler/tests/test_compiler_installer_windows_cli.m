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
ncc('--help');
if ~ispc(); return; end
try
check_installer();

catch installer_error
  skip_installer_antivirus(installer_error);
end

function check_installer()
  scratchBefore = dir(fullfile(tempdir(), 'nelson-install-*'));
  work = [tempname(), ' installer flags '' ', char([28450, 23383])]; mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  entry = fullfile(work, 'windows_cli_app.m');
  filewrite(entry, 'function windows_cli_app(); disp(''WINDOWS_INSTALLER_CLI_OK''); end');
  result = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(work, 'build'), 'EmbedArchive', false);
  asserts.isfalse(any(contains(result.RuntimeDependencies.Required.RelativePath, 'nelson-installer')));
  options = compiler.package.InstallerOptions(result, 'RuntimeDelivery', 'none', ...
    'InstallerName', 'installer', 'OutputDir', fullfile(work, 'package'), ...
    'DefaultInstallationDir', fullfile(work, 'default'));
  compiler.package.installer(result, 'Options', options);
  setup = fullfile(options.OutputDir, 'installer.exe');
  appDigest = sha256(result.Files{1}, '-file');
  rmdir(result.Options.OutputDir, 's'); rmfile(entry);
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  native = ' /CURRENTUSER /PORTABLE=1';
  destination = fullfile(work, 'command line');
  log = fullfile(work, 'command.log');
  args = ['-agreeToLicense yes -applicationFolder ', quoted([destination, '\']), ...
    ' -outputFile ', quoted(log), native];
  invoke(setup, args, true);
  verify_application(destination, appDigest);
  asserts.istrue(isfile(log));
  uninstall(destination);
  control = fullfile(work, 'control.txt');
  destination = fullfile(work, 'literal %NCC_INSTALLER_TEST% {value}');
  filewrite(control, [char(65279), '# no shell evaluation', char([13 10]), ...
    'agreeToLicense=yes', char([13 10]), 'applicationFolder=', destination, char([13 10]), ...
    'outputFile=', fullfile(work, 'control.log'), char([13 10]), ...
    'desktopShortcut=true', char([13 10]), 'startMenuShortcut=true', char([13 10])]);
  invoke(setup, ['-inputFile ', quoted(control), native], true);
  verify_application(destination, appDigest);
  uninstall(destination);
  destination = fullfile(work, 'combined destination');
  filewrite(control, ['# UTF-8 without BOM', newline(), 'destinationFolder=', destination, newline()]);
  invoke(setup, ['-agreeToLicense yes -inputFile ', quoted(control), native], true);
  verify_application(destination, appDigest); uninstall(destination);
  invoke(setup, ['-agreeToLicense yes', native], true);
  verify_application(options.DefaultInstallationDir, appDigest); uninstall(options.DefaultInstallationDir);
  destination = fullfile(work, 'native arguments');
  log = fullfile(work, 'native.log');
  invoke(setup, ['/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /DIR=', quoted(destination), ...
    ' /LOG=', quoted(log), native], true);
  verify_application(destination, appDigest); uninstall(destination);
  asserts.istrue(isfile(log));
  check_rejections(setup, work, native);
  check_child_failure(setup, work, native);
  check_payload(setup, work, native);
  asserts.isequal(numel(dir(fullfile(tempdir(), 'nelson-install-*'))), numel(scratchBefore));
  clear restore cleanup;
end

function check_rejections(setup, work, native)
  destination = fullfile(work, 'refused');
  prefix = ['-agreeToLicense yes -applicationFolder ', quoted(destination)];
  for args = {['-applicationFolder ', quoted(destination), ' -agreeToLicense yes'], ...
    [prefix, ' -destinationFolder ', quoted(destination)], ...
    [prefix, ' -applicationFolder ', quoted(destination)], ...
    '-agreeToLicense no', '-agreeToLicense yes -unknown value', ...
    '-agreeToLicense yes -applicationFolder relative', ...
    '-agreeToLicense yes -desktopShortcut true', ...
    '-agreeToLicense yes -runtimeFolder C:\runtime', '-agreeToLicense yes -outputFile', ...
    '-agreeToLicense yes -inputFile', '-agreeToLicense yes -applicationFolder', ...
    '-agreeToLicense yes -outputFile -applicationFolder', ...
    '-agreeToLicense yes /ALLUSERS', '-agreeToLicense yes /CURRENTUSER'}
    status = invoke(setup, [args{1}, native], false);
    asserts.isequal(status, 1);
    asserts.isfalse(isfolder(destination));
  end
  existingLog = fullfile(work, 'existing.log'); filewrite(existingLog, 'KEEP');
  invoke(setup, [prefix, ' -outputFile ', quoted(existingLog), native], false);
  asserts.isequal(fileread(existingLog), 'KEEP');
  control = fullfile(work, 'invalid.txt');
  for content = {['applicationFolder=', destination], ...
    ['agreeToLicense=yes', newline(), 'applicationFolder=', destination, newline(), 'applicationFolder=', destination], ...
    ['agreeToLicense=yes', newline(), 'desktopShortcut=maybe'], ...
    ['agreeToLicense=yes', newline(), 'unknown=value'], ['agreeToLicense=yes', char(0)]}
    filewrite(control, content{1});
    invoke(setup, ['-inputFile ', quoted(control), native], false);
    asserts.isfalse(isfolder(destination));
  end
  filewrite(control, ['agreeToLicense=yes', newline(), 'destinationFolder=', destination]);
  invoke(setup, [prefix, ' -inputFile ', quoted(control), native], false);
  invoke(setup, ['-inputFile ', quoted(control), ' -inputFile ', quoted(control), native], false);
  invoke(setup, ['-inputFile ', quoted(work), native], false);
  fid = fopen(control, 'wb'); fwrite(fid, uint8([255 254 97 0]), 'uint8'); fclose(fid);
  invoke(setup, ['-inputFile ', quoted(control), native], false);
  filewrite(control, repmat('x', 1, 1048577));
  invoke(setup, ['-inputFile ', quoted(control), native], false);
  log = fullfile(work, 'error.log');
  invoke(setup, [prefix, ' -outputFile ', quoted(log), ' -unknown value', native], false);
  asserts.istrue(contains(fileread(log), 'Unsupported installation option: unknown'));
  asserts.isfalse(isfolder(destination));
end

function check_child_failure(setup, work, native)
  first = fullfile(work, 'native failure'); second = fullfile(work, 'translated failure');
  mkdir(fullfile(first, 'windows_cli_app.exe')); mkdir(fullfile(second, 'windows_cli_app.exe'));
  statusA = invoke(setup, ['/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /DIR=', quoted(first), native], false);
  statusB = invoke(setup, ['-agreeToLicense yes -applicationFolder ', quoted(second), native], false);
  asserts.isequal(statusB, statusA);
  asserts.istrue(isfolder(fullfile(second, 'windows_cli_app.exe')));
end

function check_payload(setup, work, native)
  fid = fopen(setup, 'rb'); bytes = fread(fid, Inf, '*uint8'); fclose(fid);
  footer = numel(bytes) - 95;
  asserts.isequal(char(bytes(footer:footer + 15)'), ['NLS.INSTALLER.1', char(0)]);
  offset = double(typecast(bytes(footer + 16:footer + 23), 'uint64'));
  for kind = 1:4
    changed = bytes;
    if kind == 1
      changed(offset + 1) = 0;
    elseif kind == 2
      changed(footer + 16:footer + 23) = 0;
    elseif kind == 3
      changed = changed(1:end - 1);
    else
      changed(footer + 24:footer + 31) = 255;
    end
    broken = fullfile(work, ['broken_', int2str(kind), '.exe']);
    fid = fopen(broken, 'wb'); fwrite(fid, changed, 'uint8'); fclose(fid);
    log = fullfile(work, ['broken_', int2str(kind), '.log']);
    destination = fullfile(work, 'must not install');
    status = invoke(broken, ['-agreeToLicense yes -applicationFolder ', quoted(destination), ...
      ' -outputFile ', quoted(log), native], false);
    asserts.isequal(status, 1);
    asserts.istrue(contains(fileread(log), 'Installer payload') || contains(fileread(log), 'installer payload'));
    asserts.isfalse(isfolder(destination));
  end
  output = fullfile(work, 'do_not_replace.exe'); filewrite(output, 'KEEP');
  rejected = false;
  try
    __nelsonc_executable__('wrap-installer', setup, setup, output);
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
  asserts.isequal(fileread(output), 'KEEP');
end

function verify_application(destination, expected)
  executable = fullfile(destination, 'windows_cli_app.exe');
  asserts.isequal(sha256(executable, '-file'), expected);
  [status, output] = system(quoted(executable), 60);
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, 'WINDOWS_INSTALLER_CLI_OK'), output);
end

function uninstall(destination)
  files = dir(fullfile(destination, 'unins*.exe'));
  asserts.isequal(numel(files), 1);
  invoke(fullfile(destination, files.name), '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART', true);
  asserts.isfalse(isfile(fullfile(destination, 'windows_cli_app.exe')));
end

function status = invoke(executable, arguments, success)
  [status, output] = system([quoted(executable), ' ', arguments], 90);
  asserts.istrue(status >= 0 && status < 65536, [arguments, newline(), output]);
  asserts.isequal(status == 0, success, [arguments, newline(), output]);
end

function value = quoted(value)
  count = 0;
  while count < numel(value) && value(end - count) == '\'
    count = count + 1;
  end
  value = [value, repmat('\', 1, count)];
  value = ['"', value, '"'];
end
