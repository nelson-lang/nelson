%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

ncc('--help');
directory = tempname();
__application_archive__('directory', directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
assert_rejected(@() __application_archive__('directory', directory));
stub = fullfile(directory, 'stub');
write_bytes(stub, uint8('native-launcher'));
data = fullfile(directory, 'data.txt');
write_bytes(data, uint8('embedded application'));
archive = fullfile(directory, 'payload.zip');
zip(archive, {'data.txt'}, [directory, '/']);
executable = fullfile(directory, 'application.exe');
__nelsonc_archive__('create', stub, archive, executable, false);
original = read_bytes(executable);
assert_rejected(@() __nelsonc_archive__('create', stub, archive, executable, false));
asserts.isequal(read_bytes(executable), original);
destination = fullfile(directory, 'extracted');
__application_archive__('directory', destination);
__application_archive__('extract', executable, destination);
asserts.isequal(fileread(fullfile(destination, 'data.txt')), 'embedded application');
asserts.isfalse(isfile(fullfile(destination, '.nelson-payload.zip')));
assert_rejected(@() __application_archive__('extract', executable, destination));
emptyDestination = fullfile(directory, 'empty');
__application_archive__('directory', emptyDestination);
fingerprint = repmat('a', 1, 64);
for gui = [false, true]
  installed = fullfile(directory, ['installed', num2str(gui), '.exe']);
  __nelsonc_archive__('create-installed', stub, archive, installed, gui, fingerprint);
  installedBytes = read_bytes(installed);
  trailer = numel(installedBytes) - 352;
  asserts.isequal(installedBytes(trailer + 9), uint8(2));
  asserts.isequal(installedBytes(trailer + 13), uint8(2 + double(gui)));
  asserts.isequal(char(installedBytes(trailer + (97:160)))', fingerprint);
  extracted = fullfile(directory, ['installed', num2str(gui)]);
  __application_archive__('directory', extracted);
  __application_archive__('extract', installed, extracted);
  asserts.isequal(fileread(fullfile(extracted, 'data.txt')), 'embedded application');
  corruptInstalled = fullfile(directory, 'corrupt-installed.exe');
  for offset = [trailer + 9, trailer + 13, trailer + 97, trailer + 161]
    bytes = installedBytes;
    bytes(offset) = uint8(255);
    write_bytes(corruptInstalled, bytes);
    assert_rejected(@() __application_archive__('extract', corruptInstalled, emptyDestination));
  end
end
for invalidFingerprint = {'', 'invalid', repmat('g', 1, 64), repmat('a', 1, 65)}
  invalidInstalled = fullfile(directory, 'invalid-installed.exe');
  assert_rejected(@() __nelsonc_archive__('create-installed', stub, archive, invalidInstalled, false, invalidFingerprint{1}));
  asserts.isfalse(isfile(invalidInstalled));
end
bad = fullfile(directory, 'corrupt.exe');
for count = [0, 8, numel(original) - 1]
  write_bytes(bad, original(1:count));
  assert_rejected(@() __application_archive__('extract', bad, emptyDestination));
end
for offset = [20, numel(original) - 351, numel(original) - 343, numel(original) - 335]
  bytes = original;
  bytes(offset) = bitxor(bytes(offset), uint8(1));
  write_bytes(bad, bytes);
  assert_rejected(@() __application_archive__('extract', bad, emptyDestination));
end
for fixture = {'dotdot', 'absolute', 'drive', 'backslash'}
  source = fullfile(nelsonroot(), 'modules', 'file_archiver', 'tests', ['test_zipslip_', fixture{1}, '.zip']);
  target = fullfile(directory, [fixture{1}, '.exe']);
  __nelsonc_archive__('create', stub, source, target, false);
  assert_rejected(@() __application_archive__('extract', target, emptyDestination));
end
asserts.isfalse(isfile(fullfile(directory, 'zipslip_canary.txt')));
zipBytes = read_bytes(archive);
central = find(zipBytes(1:end - 3) == 80 & zipBytes(2:end - 2) == 75 ...
  & zipBytes(3:end - 1) == 1 & zipBytes(4:end) == 2);
asserts.isequal(numel(central), 1);
for variant = 1:5
  bytes = zipBytes;
  if variant == 1
    bytes(central + 16) = bitxor(bytes(central + 16), uint8(1));
  elseif variant == 2
    bytes(central + 24:central + 27) = uint8([1; 0; 0; 32]);
  elseif variant == 3
    bytes(central + 24:central + 27) = uint8([1; 0; 0; 0]);
  elseif variant == 4
    bytes(central + 8) = bitor(bytes(central + 8), uint8(1));
  else
    bytes(central + 46:central + 53) = uint8('COM1.txt')';
  end
  malformedZip = fullfile(directory, 'malformed.zip');
  write_bytes(malformedZip, bytes);
  malformedExe = fullfile(directory, ['malformed', num2str(variant), '.exe']);
  __nelsonc_archive__('create', stub, malformedZip, malformedExe, false);
  malformedDestination = fullfile(directory, ['malformed', num2str(variant)]);
  __application_archive__('directory', malformedDestination);
  assert_rejected(@() __application_archive__('extract', malformedExe, malformedDestination));
end
clear cleanup;

function bytes = read_bytes(filename)
  fid = fopen(filename, 'rb');
  bytes = fread(fid, Inf, '*uint8');
  fclose(fid);
end

function write_bytes(filename, bytes)
  fid = fopen(filename, 'wb');
  fwrite(fid, bytes, 'uint8');
  fclose(fid);
end

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
