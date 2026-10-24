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
__application_archive__('directory', directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
stub = fullfile(directory, 'stub');
filewrite(stub, 'native-launcher');
filewrite(fullfile(directory, 'data.txt'), 'EXTERNAL_DATA');
archive = fullfile(directory, 'payload.zip');
zip(archive, {'data.txt'}, [directory, '/']);
for installed = [false, true]
  for gui = [false, true]
    stem = ['app_', num2str(installed), num2str(gui)];
    executable = fullfile(directory, [stem, '.exe']);
    external = fullfile(directory, [stem, '.nca']);
    operation = 'create-external';
    extra = {};
    if installed
      operation = 'create-external-installed';
      extra = {repmat('a', 1, 64)};
    end
    __nelsonc_archive__(operation, stub, archive, executable, gui, extra{:});
    bytes = read_bytes(executable);
    asserts.isequal(numel(bytes), numel(read_bytes(stub)) + 352);
    asserts.isequal(bytes(end - 343), uint8(3));
    asserts.isequal(bytes(end - 339), uint8(4 + 2 * installed + gui));
    asserts.isequal(read_bytes(external), read_bytes(archive));
    __application_archive__('verify', executable);
    target = fullfile(directory, stem);
    __application_archive__('directory', target);
    __application_archive__('extract', executable, target);
    asserts.isequal(fileread(fullfile(target, 'data.txt')), 'EXTERNAL_DATA');
    check_corruption(executable, external, bytes, directory);
  end
end
collision = fullfile(directory, 'collision.nca');
filewrite(collision, 'USER DATA');
output = fullfile(directory, 'collision.exe');
rejected(@() __nelsonc_archive__('create-external', stub, archive, output, false));
asserts.isequal(fileread(collision), 'USER DATA');
asserts.isfalse(isfile(output));
self = fullfile(directory, 'same.nca');
rejected(@() __nelsonc_archive__('create-external', stub, archive, self, false));
asserts.isfalse(isfile(self));
for fixture = {'dotdot', 'absolute', 'drive', 'backslash'}
  source = fullfile(modulepath('file_archiver', 'tests'), ['test_zipslip_', fixture{1}, '.zip']);
  output = fullfile(directory, [fixture{1}, '.exe']);
  target = fullfile(directory, [fixture{1}, '_extract']);
  __nelsonc_archive__('create-external', stub, source, output, false);
  __application_archive__('directory', target);
  rejected(@() __application_archive__('extract', output, target));
end
asserts.isfalse(isfile(fullfile(directory, 'zipslip_canary.txt')));
clear cleanup;

function check_corruption(executable, external, original, directory)
  bytes = read_bytes(external);
  for variant = 1:3
    damaged = bytes;
    if variant == 1; damaged(1) = bitxor(damaged(1), uint8(1)); end
    if variant == 2; damaged = damaged(1:end - 1); end
    if variant == 3; damaged(end + 1) = uint8(0); end
    write_bytes(external, damaged);
    rejected(@() __application_archive__('verify', executable));
    target = tempname(directory);
    __application_archive__('directory', target);
    rejected(@() __application_archive__('extract', executable, target));
    asserts.isfalse(isfile(fullfile(target, '.nelson-payload.zip')));
  end
  rmfile(external);
  rejected(@() __application_archive__('verify', executable));
  write_bytes(external, bytes);
  for offset = [9, 13, 17, 25, 33]
    damaged = original;
    damaged(end - 352 + offset) = uint8(255);
    write_bytes(executable, damaged);
    rejected(@() __application_archive__('verify', executable));
  end
  write_bytes(executable, original);
  __application_archive__('verify', executable);
end

function bytes = read_bytes(filename)
  fid = fopen(filename, 'rb');
  cleanup = onCleanup(@() fclose(fid));
  bytes = fread(fid, Inf, '*uint8');
end

function write_bytes(filename, bytes)
  fid = fopen(filename, 'wb');
  cleanup = onCleanup(@() fclose(fid));
  fwrite(fid, bytes, 'uint8');
end

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end
