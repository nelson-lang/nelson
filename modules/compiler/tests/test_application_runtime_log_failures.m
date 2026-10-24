%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ispc(); skip_testsuite('Native output bridge requires Windows.'); end
check_failures();

function check_failures()
  ncc('--help');
  directory = tempname();
  mkdir(directory);
  cleanup = onCleanup(@() rmdir(directory, 's'));
  entry = fullfile(directory, 'log_failure.m');
  filewrite(entry, 'function log_failure(); disp(''ENTRY_REACHED''); end');
  for windowed = [false, true]
    app = fullfile(directory, ['failure', int2str(windowed), '.exe']);
    log = [app, '.log'];
    options = ncc('options', entry, 'ExecutableName', ['failure', int2str(windowed)], ...
      'OutputDir', directory, 'NoConsole', windowed, 'RuntimeLogFile', log, 'Mode', 'cli');
    result = ncc(options);
    rmfile(entry);
    runtime = result.RuntimeDirectory;
    hidden = [runtime, '.hidden'];
    movefile(runtime, hidden);
    restore = onCleanup(@() movefile(hidden, runtime));
    check_failure(app, log, 'Missing application runtime.');
    clear restore;
    engine = fullfile(runtime, 'bin', 'x64', 'libnlsEngine.dll');
    fid = fopen(engine, 'rb'); bytes = fread(fid, Inf, '*uint8'); fclose(fid);
    filewrite(engine, 'invalid library');
    check_failure(app, log, 'Windows error 193');
    signature = uint8('ConfigureNelsonApplicationOutputV1')';
    altered = bytes;
    matches = 0;
    for candidate = find(bytes == signature(1))'
      last = candidate + numel(signature) - 1;
      if last <= numel(bytes) && isequal(bytes(candidate:last), signature)
        altered(candidate) = uint8('X'); matches = matches + 1;
      end
    end
    asserts.istrue(matches > 0);
    write_bytes(engine, altered);
    check_failure(app, log, 'output capture interface V1 is missing.');
    write_bytes(engine, bytes);
    [status, output] = system(['"', app, '"'], 60);
    asserts.isequal(status, 0, output);
    asserts.istrue(contains(fileread(log), 'ENTRY_REACHED'));
    rejected(@() __nelsonc_executable__('set-log-file', app, 'changed.log'));
    filewrite(entry, 'function log_failure(); disp(''ENTRY_REACHED''); end');
  end
  clear cleanup;
end

function check_failure(app, log, marker)
  if isfile(log); rmfile(log); end
  [status, output] = system(['"', app, '" 2>&1'], 30);
  asserts.isequal(status, 2, output);
  asserts.istrue(isfile(log));
  text = fileread(log);
  asserts.istrue(contains(text, marker), text);
  asserts.isfalse(contains(text, 'ENTRY_REACHED'));
end
function write_bytes(filename, bytes)
  fid = fopen(filename, 'wb');
  closer = onCleanup(@() fclose(fid));
  fwrite(fid, bytes, 'uint8');
end
function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end
