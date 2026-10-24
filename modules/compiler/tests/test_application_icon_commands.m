%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ispc(); skip_testsuite('Native icon resources require Windows.'); end
check_commands();

function check_commands()
  ncc('--help');
  directory = tempname();
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  gateway = fullfile(modulepath('nelson', 'builtin'), ['libnlsGraphics_io_builtin', getdynlibext()]);
  [name, functions, state] = gatewayinfo(gateway);
  if strcmp(state, 'not_loaded')
    addgateway(gateway, 'loaded');
    restore = onCleanup(@() removegateway(gateway));
  end
  picture = fullfile(directory, 'small.png');
  imwrite(uint8(ones(8, 16, 3) * 80), picture);
  entry = fullfile(directory, 'icon_command.m');
  filewrite(entry, 'function icon_command(); disp(''ICON_COMMAND_OK''); end');
  executable = fullfile(directory, 'command.exe');
  binary = modulepath('nelson', 'bin');
  tool = fullfile(binary, 'nelsonc.exe');
  [status, output] = system(['"', tool, '" "', entry, ...
    '" --executable-icon "', picture, '" --cli --runtime installed -o "', executable, '"'], 90);
  asserts.isequal(status, 0, output);
  asserts.isfalse(contains(output, 'Please check'));
  minimal = fullfile(binary, 'NelSon-cli.exe');
  script = fullfile(directory, 'check_icon_gateway.m');
  target = fullfile(directory, 'minimal.exe');
  invalid = fullfile(directory, 'invalid.png');
  filewrite(invalid, 'invalid');
  lines = {'ncc(''--help'');', ...
    'gateway = fullfile(modulepath(''nelson'', ''builtin''), [''libnlsGraphics_io_builtin'', getdynlibext()]);', ...
    'resizeGateway = fullfile(modulepath(''nelson'', ''builtin''), [''libnlsImage_processing_builtin'', getdynlibext()]);', ...
    'removegateway(resizeGateway);', ...
    'readerModule = ismodule(''graphics_io''); resizeModule = ismodule(''image_processing'');', ...
    '[name, list, initial] = gatewayinfo(gateway);', ...
    ['ncc(', quoted(entry), ', ''--executable-icon'', ', quoted(picture), ...
    ', ''--cli'', ''--runtime'', ''installed'', ''-o'', ', quoted(target), ');'], ...
    '[name, list, after] = gatewayinfo(gateway);', 'asserts.isequal(after, initial);', ...
    '[name, list, resized] = gatewayinfo(resizeGateway);', 'asserts.isequal(resized, ''not_loaded'');', ...
    'failed = false;', ['try; ncc(', quoted(entry), ', ''--executable-icon'', ', quoted(invalid), ...
    ', ''--runtime'', ''installed'', ''-o'', ', quoted([target, '.failed.exe']), ...
    '); catch; failed = true; end'], 'asserts.istrue(failed);', ...
    '[name, list, after] = gatewayinfo(gateway);', 'asserts.isequal(after, initial);', ...
    '[name, list, resized] = gatewayinfo(resizeGateway);', 'asserts.isequal(resized, ''not_loaded'');', ...
    'asserts.isequal(ismodule(''graphics_io''), readerModule);', ...
    'asserts.isequal(ismodule(''image_processing''), resizeModule);', ...
    'disp(''ICON_GATEWAY_RESTORED'');', 'exit(0);'};
  filewrite(script, strjoin(lines, newline()));
  [status, output] = system(['"', minimal, ...
    '" --no-ipc --no-user-modules --no-user-startup --quiet --execute "run(', quoted(script), ');"'], 90);
  asserts.isequal(status, 0, output);
  asserts.istrue(contains(output, 'ICON_GATEWAY_RESTORED'), output);
  old = getenv('NELSONC_RUNTIME_ROOT');
  restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', old));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  rmfile(entry);
  rmfile(picture);
  for app = {executable, target}
    [status, output] = system(['"', app{1}, '"'], 60);
    asserts.isequal(status, 0, output);
    asserts.istrue(contains(output, 'ICON_COMMAND_OK'));
  end
  clear restoreRuntime;
  clear cleanup;
end

function result = quoted(value)
  result = ['''', strrep(value, '''', ''''''), ''''];
end
