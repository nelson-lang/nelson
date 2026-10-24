%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
if ~ispc(); skip_testsuite('Native icon resources require Windows.'); end
check_icons();

function check_icons()
  ncc('--help');
  gateway = fullfile(modulepath('nelson', 'builtin'), ['libnlsGraphics_io_builtin', getdynlibext()]);
  [name, functions, state] = gatewayinfo(gateway);
  if strcmp(state, 'not_loaded')
    addgateway(gateway, 'loaded');
    restoreGateway = onCleanup(@() removegateway(gateway));
  end
  directory = [tempname(), ' icon ', char([28450, 23383])];
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  entry = fullfile(directory, 'icon_entry.m');
  filewrite(entry, 'function icon_entry(); disp(''ICON_ENTRY_OK''); end');
  rgb = zeros(48, 48, 3, 'uint8');
  rgb(:, 1:24, 1) = 255;
  rgb(:, 25:48, 3) = 255;
  alpha = uint8(255 * ones(48, 48));
  alpha(1:12, :) = 0;
  alpha(13:24, :) = 128;
  icon = fullfile(directory, 'picture.png');
  imwrite(rgb, icon, 'Alpha', alpha);
  options = compiler.build.StandaloneApplicationOptions(entry, 'ExecutableIcon', icon, ...
    'ExecutableVersion', '2.3', 'RuntimeLogFile', 'runtime.log', 'OutputDir', fullfile(directory, 'console'));
  stub = fullfile(modulepath('nelson', 'bin'), 'nelson-app.exe');
  stubDigest = sha256(stub, '-file');
  result = compiler.build.standaloneApplication(options);
  options.OutputDir = fullfile(directory, 'repeat');
  again = compiler.build.standaloneApplication(options);
  asserts.isequal(sha256(result.Files{1}, '-file'), sha256(again.Files{1}, '-file'));
  asserts.isequal(sha256(stub, '-file'), stubDigest);
  asserts.isequal(result.Options.ExecutableIcon, strrep(icon, '\', '/'));
  snapshot = result.RuntimeDependencies.__nelsonc_snapshot__();
  asserts.isfalse(snapshot.gui);
  asserts.isfalse(any(strcmp(snapshot.modules, 'graphics_io')));
  asserts.isfalse(any(strcmp(snapshot.modules, 'image_processing')));
  inspect(result, fullfile(directory, 'extracted'));
  [actual, map, actualAlpha] = imread(fullfile(directory, 'extracted', 'icon-48.png'));
  asserts.isequal(actualAlpha, alpha);
  asserts.isequal(actual(13:end, :, :), rgb(13:end, :, :));
  check_replacement(stub, icon, directory);
  check_geometry(directory);
  options.OutputDir = fullfile(directory, 'windowed');
  windowed = compiler.build.standaloneWindowsApplication(options);
  inspect(windowed, fullfile(directory, 'windowed_icons'));
  for extension = {'.jpg', '.jpeg', '.bmp', '.gif'}
    source = fullfile(directory, ['picture', extension{1}]);
    imwrite(rgb, source);
    options.ExecutableIcon = source;
    options.OutputDir = fullfile(directory, ['format_', extension{1}(2:end)]);
    built = compiler.build.standaloneApplication(options);
    inspect(built, fullfile(options.OutputDir, 'extracted'));
  end
  check_failure(entry, icon, directory);
  old = getenv('NELSONC_RUNTIME_ROOT');
  restore = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  rmfile(entry);
  rmfile(icon);
  for application = {result, windowed}
    [status, output] = system(['"', application{1}.Files{1}, '"'], 60);
    asserts.isequal(status, 0, output);
    asserts.istrue(contains(output, 'ICON_ENTRY_OK'));
    asserts.istrue(contains(fileread(fullfile(application{1}.Options.OutputDir, 'runtime.log')), 'ICON_ENTRY_OK'));
  end
  clear restore;
  clear cleanup;
end

function check_replacement(stub, icon, directory)
  frames = __nelsonc_icon_files__(icon, directory);
  target = fullfile(directory, 'replace.exe');
  copyfile(stub, target);
  __nelsonc_executable__('set-icon', target, frames);
  digest = sha256(target, '-file');
  __nelsonc_executable__('set-icon', target, frames);
  asserts.isequal(sha256(target, '-file'), digest);
  filewrite(frames{1}, 'invalid png');
  rejected(@() __nelsonc_executable__('set-icon', target, frames));
  asserts.isequal(sha256(target, '-file'), digest);
end

function inspect(result, output)
  script = fullfile(modulepath('compiler'), 'tests', 'test_application_icon_resources.ps1');
  [status, text] = system(['powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File "', script, '" -Executable "', ...
    result.Files{1}, '" -OutputDirectory "', output, '"'], 60);
  asserts.isequal(status, 0, text);
end

function check_geometry(directory)
  source = fullfile(directory, 'gray.png');
  imwrite(uint8(120 * ones(8, 16)), source);
  frames = __nelsonc_icon_files__(source, directory);
  [pixels, map, alpha] = imread(frames{3});
  asserts.isequal(size(pixels), [32, 32, 3]);
  asserts.isequal(pixels(9:24, :, :), uint8(120 * ones(16, 32, 3)));
  expected = zeros(32, 32, 'uint8');
  expected(9:24, :) = 255;
  asserts.isequal(alpha, expected);
  rgb = zeros(1, 2, 3, 'uint8');
  rgb(1, 2, 1) = 255;
  imwrite(rgb, source, 'Alpha', uint8([0, 255]));
  frames = __nelsonc_icon_files__(source, directory);
  [pixels, map, alpha] = imread(frames{3});
  red = pixels(:, :, 1);
  asserts.isequal(red(alpha > 0), uint8(255 * ones(nnz(alpha > 0), 1)));
  asserts.isequal(pixels(:, :, 2:3), zeros(32, 32, 2, 'uint8'));
end

function check_failure(entry, icon, directory)
  for value = {42, {'icon.png'}, fullfile(directory, 'missing.png'), [icon, char(0)]}
    rejected(@() compiler.build.StandaloneApplicationOptions(entry, 'ExecutableIcon', value{1}));
  end
  invalid = fullfile(directory, 'invalid.png');
  filewrite(invalid, 'not an image');
  out = fullfile(directory, 'failed');
  rejected(@() compiler.build.standaloneApplication(entry, 'ExecutableIcon', invalid, 'OutputDir', out));
  asserts.isfalse(isfolder(out));
  rejected(@() ncc(entry, '--executable-icon'));
end

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end
