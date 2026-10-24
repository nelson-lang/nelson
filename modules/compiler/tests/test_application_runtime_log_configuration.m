%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
extension = '';
if ispc(); extension = '.exe'; end
stub = fullfile(modulepath('nelson', 'bin'), ['nelson-app', extension]);
bytes = read_bytes(stub);
marker = locate(bytes, uint8(['NLSSTARTUPCONF01', char([40, 0, 0, 0])])');
% The signature has 16 bytes, including the S ending NLS and starting STARTUP.
asserts.isequal(numel(marker), 1);
launcher = locate(bytes, uint8(['NLSLAUNCHERINFO1', char([48, 0, 0, 0])])');
asserts.isequal(numel(launcher), 1);
target = fullfile(directory, ['configured', extension]);
write_bytes(target, bytes);
__nelsonc_executable__('set-log-file', target, 'output.log');
configured = read_bytes(target);
if ismac()
  % macOS appends the configuration plus a 24-byte footer instead of
  % rewriting the signed Mach-O in place, so 'output.log' precedes the footer.
  asserts.isequal(numel(configured), numel(bytes) + 18 + 24);
  asserts.isequal(char(configured((end - 33):(end - 24)))', 'output.log');
else
  asserts.isequal(numel(configured), numel(bytes) + 18);
  asserts.isequal(char(configured((end - 9):end))', 'output.log');
end
changed = bytes;
changed(marker + (24:39)) = configured(marker + (24:39));
asserts.isequal(configured(1:numel(bytes)), changed);
rejected(@() __nelsonc_executable__('set-log-file', target, 'again.log'));
asserts.isequal(read_bytes(target), configured);
for index = [0, 16, 20, 24, 32]
  invalid = bytes;
  invalid(marker + index) = uint8(255);
  write_bytes(target, invalid);
  rejected(@() __nelsonc_executable__('set-log-file', target, 'output.log'));
  asserts.isequal(read_bytes(target), invalid);
end
write_bytes(target, [bytes; bytes(marker:(marker + 39))]);
rejected(@() __nelsonc_executable__('set-log-file', target, 'output.log'));
write_bytes(target, bytes(1:(marker + 38)));
rejected(@() __nelsonc_executable__('set-log-file', target, 'output.log'));
legacy = bytes;
legacy(launcher + 20) = 1;
write_bytes(target, legacy);
info = jsondecode(__nelsonc_executable__('launcher-contract', target));
asserts.isequal(info.format, 1);
rejected(@() __nelsonc_executable__('set-log-file', target, 'output.log'));
rejected(@() __nelsonc_executable__('set-log-file', stub, ''));
asserts.isequal(read_bytes(stub), bytes);
clear cleanup;

function positions = locate(bytes, signature)
  positions = [];
  for candidate = find(bytes == signature(1))'
    last = candidate + numel(signature) - 1;
    if last <= numel(bytes) && isequal(bytes(candidate:last), signature)
      positions(end + 1) = candidate;
    end
  end
end
function bytes = read_bytes(filename)
  fid = fopen(filename, 'rb');
  closer = onCleanup(@() fclose(fid));
  bytes = fread(fid, Inf, '*uint8');
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
