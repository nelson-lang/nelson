%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ispc()
  skip_testsuite('Native loader system codes require Windows.');
end
check_diagnostics();

function check_diagnostics()
  ncc('--help');
  directory = [tempname(), ' loader ', char([28450, 23383])];
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  entry = fullfile(directory, 'loader_entry.m');
  filewrite(entry, 'function loader_entry(); disp(''LOADER_ENTRY_OK''); end');
  application = fullfile(directory, 'loader.exe');
  ncc(entry, '--cli', '--runtime', 'bundled', '-o', application);
  rmfile(entry);
  [unused, platform] = fileparts(modulepath('nelson', 'builtin'));
  libraryDirectory = fullfile(directory, 'loader.runtime', 'bin', platform);
  % Damage only this application's copied runtime, never loaded host libraries.
  engine = fullfile(libraryDirectory, 'libnlsEngine.dll');
  interpreter = fullfile(libraryDirectory, 'libnlsInterpreter.dll');
  originalEngine = read_bytes(engine);
  renamedEngine = originalEngine;
  signature = uint8('StartNelsonApplicationV1')';
  matches = 0;
  for candidate = find(originalEngine == signature(1))'
    last = candidate + numel(signature) - 1;
    if last <= numel(originalEngine) && isequal(originalEngine(candidate:last), signature)
      renamedEngine(candidate) = uint8('X');
      matches = matches + 1;
    end
  end
  asserts.istrue(matches > 0);
  bytes = read_bytes(application);
  trailer = bytes((end - 351):end);
  offset = sum(double(trailer(17:24))' .* (256 .^ (0:7)));
  archive = fullfile(directory, 'payload.zip');
  write_bytes(archive, bytes((offset + 1):(end - 352)));
  for launcher = {'nelson-app.exe', 'nelson-app-windowed.exe'}
    rmfile(application);
    stub = fullfile(modulepath('nelson', 'bin'), launcher{1});
    __nelsonc_archive__('create', stub, archive, application, false);
    write_bytes(engine, uint8('invalid native runtime'));
    assert_failure(application, engine, 193, 'Cannot load application runtime.');
    write_bytes(engine, renamedEngine);
    assert_failure(application, engine, 127, 'native startup interface V1 is missing.');
    write_bytes(engine, originalEngine);
    hidden = [interpreter, '.hidden'];
    movefile(interpreter, hidden);
    restoreLibrary = onCleanup(@() movefile(hidden, interpreter));
    assert_failure(application, engine, 126, 'Cannot load application runtime.');
    clear restoreLibrary;
    [status, output] = system(['"', application, '" 2>&1'], 60);
    asserts.isequal(status, 0, output);
    asserts.istrue(contains(output, 'LOADER_ENTRY_OK'), output);
  end
  clear cleanup;
end

function assert_failure(application, engine, code, message)
  log = [application, '.stderr'];
  [status, shellOutput] = system(['"', application, '" >"', log, '" 2>&1'], 30);
  asserts.istrue(isfile(log), shellOutput);
  output = fileread(log, 'char', 'unix', 'UTF-8');
  asserts.isequal(status, 2, output);
  asserts.istrue(contains(output, message), output);
  asserts.istrue(contains(output, ['Windows error ', num2str(code)]), output);
  asserts.istrue(contains(strrep(output, '\', '/'), strrep(engine, '\', '/')), output);
  asserts.isfalse(contains(output, 'LOADER_ENTRY_OK'));
end

function bytes = read_bytes(filename)
  fid = fopen(filename, 'rb');
  asserts.istrue(fid >= 0);
  closer = onCleanup(@() fclose(fid));
  bytes = fread(fid, Inf, '*uint8');
  clear closer;
end

function write_bytes(filename, bytes)
  fid = fopen(filename, 'wb');
  asserts.istrue(fid >= 0);
  closer = onCleanup(@() fclose(fid));
  asserts.isequal(fwrite(fid, bytes, 'uint8'), numel(bytes));
  clear closer;
end
