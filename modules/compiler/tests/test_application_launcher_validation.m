%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
oldRuntime = getenv('NELSONC_RUNTIME_ROOT');
restoreEnvironment = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', oldRuntime));
entry = fullfile(directory, 'reader_entry.m');
filewrite(entry, 'function reader_entry(); disp(''READER_ENTRY_EXECUTED''); end');
extension = '';
if ispc()
  extension = '.exe';
end
application = fullfile(directory, ['reader', extension]);
ncc(entry, '--cli', '--runtime', 'installed', '-o', application);
fid = fopen(application, 'rb');
original = fread(fid, Inf, '*uint8');
fclose(fid);
trailer = numel(original) - 352;
extraction = fullfile(directory, 'invalid-extraction');
__application_archive__('directory', extraction);
% Invalid trailers must fail before attempting to locate this absent runtime.
setenv('NELSONC_RUNTIME_ROOT', fullfile(directory, 'absent-runtime'));
reasons = {'Invalid application archive magic.', 'Incompatible application archive version.', ...
  'Invalid application archive flags.', 'Invalid application archive bounds.', ...
  'Invalid application archive bounds.', 'Invalid application archive digest.', ...
  'Invalid application engine fingerprint.', 'Invalid application runtime identifier padding.', ...
  'Invalid application runtime directory.'};
for k = 1:numel(reasons)
  bytes = original;
  switch k
    case 1
      bytes(trailer + 1) = uint8('X');
    case 2
      bytes(trailer + 9:trailer + 12) = uint8([99; 0; 0; 0]);
    case 3
      bytes(trailer + 13) = uint8(4);
    case 4
      bytes(trailer + 17:trailer + 24) = uint8(0);
    case 5
      bytes(trailer + 25:trailer + 32) = uint8(0);
    case 6
      bytes(trailer + 33) = uint8('g');
    case 7
      bytes(trailer + 97) = uint8('g');
    case 8
      bytes(end) = uint8(1);
    case 9
      bytes(trailer + 9) = uint8(1);
      bytes(trailer + 13) = uint8(0);
      bytes(trailer + 97) = uint8('/');
  end
  malformed = fullfile(directory, ['invalid_', num2str(k), extension]);
  copyfile(application, malformed);
  fid = fopen(malformed, 'wb');
  fwrite(fid, bytes, 'uint8');
  fclose(fid);
  rejected = false;
  diagnostic = '';
  try
    __application_archive__('extract', malformed, extraction);
  catch ex
    diagnostic = ex.message;
    rejected = contains(ex.message, reasons{k});
  end
  asserts.istrue(rejected, diagnostic);
  [status, output] = system(['"', malformed, '" 2>&1'], 30);
  asserts.isequal(status, 2, output);
  asserts.istrue(contains(output, reasons{k}), output);
  asserts.isfalse(contains(output, 'READER_ENTRY_EXECUTED'));
end
clear restoreEnvironment cleanup;
