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
if ispc()
  extension = '.exe';
end
stub = fullfile(modulepath('nelson', 'bin'), ['nelson-app', extension]);
digest = sha256(stub, '-file');
info = contract(stub);
asserts.isequal(info.format, 2);
asserts.isequal(info.startupInterface, 1);
asserts.isequal(info.archiveMinVersion, 1);
asserts.isequal(info.archiveMaxVersion, 3);
discoveryVersion = 2;
if ~ispc(); discoveryVersion = 3; end
asserts.isequal(info.runtimeDiscoveryVersion, discoveryVersion);
asserts.isequal(info.kind, 'console');
asserts.isequal(sha256(stub, '-file'), digest);
unicode = fullfile(directory, ['launcher space ', char([28450, 23383]), extension]);
copyfile(stub, unicode);
asserts.isequal(contract(unicode), info);
asserts.isequal(contract(string(unicode)), info);
tool = contract(fullfile(modulepath('nelson', 'bin'), ['nelsonc', extension]));
asserts.isequal(tool.kind, 'compiler');
if ispc()
  windowed = contract(fullfile(modulepath('nelson', 'bin'), 'nelson-app-windowed.exe'));
  asserts.isequal(windowed.kind, 'windowed');
end
fid = fopen(stub, 'rb');
bytes = fread(fid, Inf, '*uint8');
fclose(fid);
signature = uint8(['NLSLAUNCHERINFO1', char([48, 0, 0, 0])])';
marker = [];
for candidate = find(bytes == signature(1))'
  if candidate + 19 <= numel(bytes) && isequal(bytes(candidate:(candidate + 19)), signature)
    marker(end + 1) = candidate;
  end
end
asserts.isequal(numel(marker), 1);
offset = marker - 1;
invalid = fullfile(directory, 'invalid');
for position = [1, 17, 21, 25, 29, 33, 37, 41, 45]
  changed = bytes;
  changed(offset + position) = uint8(255);
  write_bytes(invalid, changed);
  rejected(@() contract(invalid));
end
write_bytes(invalid, bytes(1:(offset + 47)));
rejected(@() contract(invalid));
write_bytes(invalid, [bytes; bytes(marker:(marker + 47))]);
rejected(@() contract(invalid));
write_bytes(invalid, uint8('LEGACY LAUNCHER'));
rejected(@() contract(invalid));
rejected(@() contract(fullfile(directory, 'missing')));
rejected(@() contract([stub, char(0)]));

entry = fullfile(directory, 'contract_entry.m');
filewrite(entry, 'function contract_entry(); disp(''CONTRACT_OK''); end');
result = compiler.build.standaloneApplication(entry, 'OutputDir', fullfile(directory, 'build'));
asserts.isequal(contract(result.Files{1}), info);
report = result.__nelsonc_report__();
asserts.isequal(report.Launcher, info);
asserts.isequal(report.Distribution.launcher, info);

% A record in the archive overlay must not be mistaken for native metadata.
fid = fopen(result.Files{1}, 'rb');
application = fread(fid, Inf, '*uint8');
fclose(fid);
trailer = application((end - 351):end);
payloadSize = sum(double(trailer(25:32))' .* (256 .^ (0:7))) + 48;
for k = 0:7
  trailer(25 + k) = uint8(mod(floor(payloadSize / 256 ^ k), 256));
end
overlay = fullfile(directory, 'overlay');
write_bytes(overlay, [application(1:(end - 352)); bytes(marker:(marker + 47)); trailer]);
asserts.isequal(contract(overlay), info);

if ispc()
  forged = report.Distribution;
  forged.buildType = 'standaloneWindowsApplication';
  forged.launcher.kind = 'windowed';
  forged = jsondecode(__nelsonc_plan__('build-report', jsonencode(forged)));
  stage = fullfile(directory, 'installer_stage');
  mkdir(stage);
  inventory = struct('files', struct('relative', forged.executable, ...
    'source', result.Files{1}, 'sha256', sha256(result.Files{1}, '-file')), 'directories', {{}});
  rejected(@() __nelsonc_installer_payload__(stage, inventory, forged, struct()), 'launcher contract does not match');
end
clear cleanup;

function value = contract(filename)
  value = jsondecode(__nelsonc_executable__('launcher-contract', filename));
end

function write_bytes(filename, bytes)
  fid = fopen(filename, 'wb');
  closer = onCleanup(@() fclose(fid));
  fwrite(fid, bytes, 'uint8');
end

function rejected(callback, message)
  failed = false;
  try
    callback();
  catch ex
    failed = startsWith(ex.identifier, 'Nelson:nelsonc:');
    asserts.istrue(failed, ex.message);
    if nargin() > 1
      asserts.istrue(contains(ex.message, message), ex.message);
    end
  end
  asserts.istrue(failed);
end
