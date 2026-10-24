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
check_runtime_installer();

catch installer_error
  skip_installer_antivirus(installer_error);
end
function check_runtime_installer()
  scratchBefore = numel(dir(fullfile(tempdir(), 'nelson-install-*')));
  work = [tempname(tempdir()), ' runtime flags '' ', char([28450, 23383])]; mkdir(work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  entry = fullfile(work, 'runtime_cli_app.m');
  filewrite(entry, 'function runtime_cli_app(proof); if ~isdeployed() || ismodule(''compiler''); error(''Invalid runtime.''); end; filewrite(proof, nelsonroot()); disp(''RUNTIME_INSTALLER_CLI_OK''); end');
  result = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(work, 'build'));
  plan = __nelsonc_distribution_plan__(result);
  output = fullfile(work, 'package');
  compiler.runtime.customInstaller('runtime', result, 'RuntimeDelivery', 'installer', 'OutputDir', output);
  setup = fullfile(output, 'runtime.exe');
  app = fullfile(work, 'runtime_cli_app.exe'); copyfile(result.Files{1}, app);
  rmdir(result.Options.OutputDir, 's'); rmfile(entry);
  check_legacy_launcher(setup, work);
  native = ' /CURRENTUSER /PORTABLE=1';
  destination = fullfile(work, 'runtime literal %UNDEFINED% {value}');
  prefix = ['-agreeToLicense yes -destinationFolder ', quoted([destination, '\'])];
  log = fullfile(work, 'command.log');
  invoke(setup, [prefix, ' -outputFile ', quoted(log), native], true);
  verify_runtime(destination, app, plan);
  asserts.istrue(isfile(log));
  control = fullfile(work, 'control.txt');
  filewrite(control, [char(65279), '# literal UTF-8 paths', char([13 10]), ...
    'agreeToLicense=yes', char([13 10]), 'destinationFolder=', destination, char([13 10]), ...
    'outputFile=', fullfile(work, 'control.log'), char([13 10])]);
  invoke(setup, ['-inputfile ', quoted(control), native], true);
  verify_runtime(destination, app, plan);
  asserts.istrue(isfile(fullfile(work, 'control.log')));
  filewrite(control, ['destinationFolder=', destination, newline()]);
  invoke(setup, ['-agreeToLicense yes -inputfile ', quoted(control), native], true);
  verify_runtime(destination, app, plan);
  check_rejections(setup, work, native);
  check_payload(setup, work, native);
  userFile = fullfile(destination, 'user.txt'); filewrite(userFile, 'KEEP');
  uninstall(destination);
  asserts.isequal(fileread(userFile), 'KEEP');
  asserts.isfalse(isfile(fullfile(destination, 'runtime.json')));
  asserts.isfalse(isfile(fullfile(destination, 'runtime-install.ini')));
  asserts.istrue(isfile(app));
  helper = fullfile(modulepath('compiler'), 'tests', 'test_runtime_installer_cli_registration.ps1');
  [status, output] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File ', quoted(helper), ' -Work ', quoted(work), ...
    ' -Fingerprint ', plan.engineFingerprint, ' -Architecture ', plan.architecture], 180);
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, 'RUNTIME_CLI_REGISTRATION_OK'), output);
  asserts.isequal(numel(dir(fullfile(tempdir(), 'nelson-install-*'))), scratchBefore);
  clear cleanup;
end

function check_legacy_launcher(setup, work)
  destination = fullfile(work, 'unsupported.exe');
  old = fullfile(modulepath('nelson', 'bin'), 'nelson-app.exe');
  rejected = false;
  try
    __nelsonc_executable__('wrap-runtime-installer', old, setup, destination);
  catch ex
    rejected = contains(ex.message, 'does not support runtime deployment arguments');
  end
  asserts.istrue(rejected);
  asserts.isfalse(isfile(destination));
end

function check_rejections(setup, work, native)
  destination = fullfile(work, 'refused');
  prefix = ['-agreeToLicense yes -destinationFolder ', quoted(destination)];
  control = fullfile(work, 'invalid.txt');
  filewrite(control, ['agreeToLicense=yes', newline(), 'destinationFolder=', destination]);
  invalid = {['-destinationFolder ', quoted(destination), ' -agreeToLicense yes'], ...
    [prefix, ' -destinationFolder ', quoted(destination)], ...
    [prefix, ' -applicationFolder ', quoted(destination)], ...
    ['-agreeToLicense yes -runtimeFolder ', quoted(destination)], ...
    ['-inputFile ', quoted(control)], ...
    ['-inputfile ', quoted(control), ' -inputfile ', quoted(control)], ...
    [prefix, ' -inputfile ', quoted(control)], ...
    '-agreeToLicense no', '-agreeToLicense yes -unknown value', ...
    '-agreeToLicense yes -destinationFolder relative', ...
    '-agreeToLicense yes -mode silent', '-agreeToLicense yes -desktopShortcut false', ...
    '-agreeToLicense yes -outputFile', '-agreeToLicense yes -inputfile', ...
    '-agreeToLicense yes -destinationFolder', '-agreeToLicense yes -outputFile -destinationFolder', ...
    '-agreeToLicense yes /ALLUSERS', '-agreeToLicense yes /CURRENTUSER'};
  for args = invalid
    asserts.isequal(invoke(setup, [args{1}, native], false), 1);
    asserts.isfalse(isfolder(destination));
  end
  for content = {['destinationFolder=', destination], ...
    ['agreeToLicense=yes', newline(), 'applicationFolder=', destination], ...
    ['agreeToLicense=yes', newline(), 'runtimeFolder=', destination], ...
    ['agreeToLicense=yes', newline(), 'desktopShortcut=false'], ...
    ['agreeToLicense=yes', newline(), 'startMenuShortcut=true'], ...
    ['agreeToLicense=yes', newline(), 'unknown=value'], ['agreeToLicense=yes', char(0)]}
    filewrite(control, content{1});
    asserts.isequal(invoke(setup, ['-inputfile ', quoted(control), native], false), 1);
    asserts.isfalse(isfolder(destination));
  end
  fid = fopen(control, 'wb'); fwrite(fid, uint8([255 254 97 0]), 'uint8'); fclose(fid);
  invoke(setup, ['-inputfile ', quoted(control), native], false);
  filewrite(control, repmat('x', 1, 1048577));
  invoke(setup, ['-inputfile ', quoted(control), native], false);
  invoke(setup, ['-inputfile ', quoted(work), native], false);
  existing = fullfile(work, 'existing.log'); filewrite(existing, 'KEEP');
  invoke(setup, [prefix, ' -outputFile ', quoted(existing), native], false);
  asserts.isequal(fileread(existing), 'KEEP');
  log = fullfile(work, 'rejected.log');
  invoke(setup, ['-agreeToLicense yes -outputFile ', quoted(log), ...
    ' -applicationFolder ', quoted(destination), native], false);
  asserts.istrue(contains(fileread(log), 'Unsupported runtime installation option'));
  asserts.isfalse(isfolder(destination));
end

function check_payload(setup, work, native)
  fid = fopen(setup, 'rb'); bytes = fread(fid, Inf, '*uint8'); fclose(fid);
  footer = numel(bytes) - 95;
  asserts.isequal(char(bytes(footer:footer + 15)'), ['NLS.RUNTIME.1', char([0 0 0])]);
  offset = double(typecast(bytes(footer + 16:footer + 23), 'uint64'));
  for kind = 1:2
    changed = bytes;
    if kind == 1; changed(offset + 1) = 0; else; changed(footer) = 0; end
    broken = fullfile(work, ['broken_', int2str(kind), '.exe']);
    fid = fopen(broken, 'wb'); fwrite(fid, changed, 'uint8'); fclose(fid);
    destination = fullfile(work, 'must not install');
    log = fullfile(work, ['broken_', int2str(kind), '.log']);
    asserts.isequal(invoke(broken, ['-agreeToLicense yes -destinationFolder ', quoted(destination), ...
      ' -outputFile ', quoted(log), native], false), 1);
    asserts.istrue(contains(fileread(log), 'Installer payload'));
    asserts.isfalse(isfolder(destination));
  end
end

function verify_runtime(destination, app, plan)
  manifest = jsondecode(fileread(fullfile(destination, 'runtime.json')));
  asserts.isequal(manifest.moduleOrder, plan.runtime.moduleOrder);
  asserts.isequal(numel(manifest.files), numel(plan.runtime.files));
  asserts.isfalse(isfile(fullfile(destination, 'runtime_cli_app.exe')));
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', destination);
  proof = fullfile(fileparts(app), 'runtime-used.txt');
  [status, output] = system([quoted(app), ' ', quoted(proof)], 60);
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, 'RUNTIME_INSTALLER_CLI_OK'), output);
  asserts.isequal(strrep(fileread(proof), '\', '/'), strrep(destination, '\', '/'));
  clear restore;
end

function uninstall(destination)
  files = dir(fullfile(destination, 'unins*.exe'));
  asserts.isequal(numel(files), 1);
  invoke(fullfile(destination, files.name), '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART', true);
end

function status = invoke(executable, arguments, success)
  [status, output] = system([quoted(executable), ' ', arguments], 90);
  asserts.istrue(status >= 0 && status < 65536, [arguments, newline(), output]);
  asserts.isequal(status == 0, success, [arguments, newline(), output]);
end

function value = quoted(value)
  count = 0;
  while count < numel(value) && value(end - count) == '\'; count = count + 1; end
  value = ['"', value, repmat('\', 1, count), '"'];
end
