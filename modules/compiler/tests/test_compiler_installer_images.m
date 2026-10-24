%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 300-->
if ~ispc(); skip_testsuite('Native installer images require Windows.'); end
ncc('--help');
try
check_images();

catch installer_error
  skip_installer_antivirus(installer_error);
end
function check_images()
  directory = [tempname(), ' images ', char([28450, 23383])];
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  source = fullfile(directory, 'source');
  mkdir(source);
  entry = fullfile(source, 'branded.m');
  filewrite(entry, 'function branded(); disp(''INSTALLER_IMAGES_OK''); end');
  make_images(source);
  result = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(directory, 'build'));
  name = ['Images-', sha256(directory)];
  options = compiler.package.InstallerOptions(result, 'ApplicationName', name, ...
    'RuntimeDelivery', 'none', 'InstallerIcon', fullfile(source, 'red.png'), ...
    'InstallerLogo', fullfile(source, 'logo.png'), ...
    'AddRemoveProgramsIcon', fullfile(source, 'blue.png'), 'Shortcut', '', ...
    'OutputDir', fullfile(directory, 'first'));
  states = image_states();
  stage = fullfile(directory, 'image-stage');
  payload = fullfile(stage, 'payload');
  mkdir(payload);
  images = __nelsonc_installer_images__(options, stage, payload);
  native('bitmap', images.InstallerLogo, '', 'red');
  compiler.package.installer(result, 'Options', options);
  first = fullfile(options.OutputDir, [options.InstallerName, '.exe']);
  options.OutputDir = fullfile(directory, 'second');
  compiler.package.installer(result, 'Options', options);
  second = fullfile(options.OutputDir, [options.InstallerName, '.exe']);
  asserts.isequal(sha256(first, '-file'), sha256(second, '-file'));
  asserts.isequal(image_states(), states);
  snapshot = result.RuntimeDependencies.__nelsonc_snapshot__();
  asserts.isfalse(any(ismember(snapshot.modules, {'graphics_io', 'image_processing'})));
  native('icon', first, '', 'red');
  check_failure(result, options, source, directory);
  asserts.isequal(image_states(), states);
  digest = sha256(result.Files{1}, '-file');
  rmdir(source, 's');
  rmdir(result.Options.OutputDir, 's');
  for portable = [true, false]
    target = fullfile(directory, ['installed_', int2str(double(portable))]);
    flags = '';
    if portable; flags = ' /PORTABLE=1'; end
    log = fullfile(directory, 'install.log');
    [status, text] = system(['"', first, '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP- /CURRENTUSER', ...
      flags, ' /DIR="', target, '" /LOG="', log, '"'], 90);
    if status ~= 0 && isfile(log); text = fileread(log); end
    asserts.isequal(status, 0, text);
    uninstallOnFailure = onCleanup(@() cleanup_installation(target));
    native('icon', fullfile(target, 'nelson-installer-assets', 'InstallerIcon.ico'), '', 'red');
    native('icon', fullfile(target, 'nelson-installer-assets', 'AddRemoveProgramsIcon.ico'), '', 'blue');
    mode = 'registered';
    if portable; mode = 'unregistered'; end
    native(mode, target, name, 'red');
    executable = fullfile(target, 'branded.exe');
    asserts.isequal(sha256(executable, '-file'), digest);
    old = getenv('NELSONC_RUNTIME_ROOT');
    restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
    setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
    [status, text] = system(['"', executable, '"'], 60);
    asserts.isequal(status, 0, text);
    asserts.istrue(contains(text, 'INSTALLER_IMAGES_OK'));
    clear restore;
    uninstall = dir(fullfile(target, 'unins*.exe'));
    asserts.isequal(numel(uninstall), 1);
    [status, text] = system(['"', fullfile(target, uninstall.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90);
    asserts.isequal(status, 0, text);
    asserts.isfalse(isfolder(fullfile(target, 'nelson-installer-assets')));
    native('unregistered', target, name, 'red');
    clear uninstallOnFailure;
  end
  clear cleanup;
end

function make_images(directory)
  gateway = fullfile(modulepath('nelson', 'builtin'), ['libnlsGraphics_io_builtin', getdynlibext()]);
  [name, functions, state] = gatewayinfo(gateway);
  if strcmp(state, 'not_loaded')
    addgateway(gateway, 'loaded');
    restore = onCleanup(@() removegateway(gateway));
  end
  red = zeros(48, 48, 3, 'uint8');
  red(:, :, 1) = 255;
  blue = zeros(48, 48, 3, 'uint8');
  blue(:, :, 3) = 255;
  imwrite(red, fullfile(directory, 'red.png'));
  imwrite(blue, fullfile(directory, 'blue.png'));
  logo = zeros(290, 112, 3, 'uint8');
  logo(1:145, :, 2) = 255;
  logo(146:end, :, 1:2) = 255;
  alpha = uint8(255 * ones(290, 112));
  alpha(1:145, :) = 128;
  imwrite(logo, fullfile(directory, 'logo.png'), 'Alpha', alpha);
end

function states = image_states()
  states = {};
  for module = {'Graphics_io', 'Image_processing'}
    path = fullfile(modulepath('nelson', 'builtin'), ['libnls', module{1}, '_builtin', getdynlibext()]);
    [name, functions, states{end + 1}] = gatewayinfo(path);
  end
end

function check_failure(result, options, source, directory)
  invalid = fullfile(source, 'invalid.png');
  filewrite(invalid, 'not an image');
  options.OutputDir = fullfile(directory, 'failed');
  options.InstallerLogo = invalid;
  failed = false;
  try; compiler.package.installer(result, 'Options', options); catch ex; failed = ~isempty(ex.message); end
  asserts.istrue(failed);
  asserts.isfalse(isfolder(options.OutputDir));
  options.InstallerLogo = '';
  collision = fullfile(source, 'nelson-installer-assets');
  mkdir(collision);
  options.AdditionalFiles = collision;
  failed = false;
  try; compiler.package.installer(result, 'Options', options); catch ex; failed = ~isempty(ex.message); end
  asserts.istrue(failed);
  asserts.isfalse(isfolder(options.OutputDir));
end

function native(mode, target, applicationName, color)
  helper = fullfile(modulepath('compiler', 'tests'), 'test_installer_image_native.ps1');
  [status, text] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', helper, '" -Mode ', mode, ...
    ' -Target "', target, '" -ApplicationName "', applicationName, '" -Color ', color], 60);
  asserts.isequal(status, 0, text);
end

function cleanup_installation(target)
  files = dir(fullfile(target, 'unins*.exe'));
  if numel(files) == 1
    system(['"', fullfile(target, files.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90);
  end
end
