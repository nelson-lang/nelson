%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
check_runtime_archive();

function check_runtime_archive()
  loaded = ismodule('compiler');
  work = tempname();
  __application_archive__('directory', work);
  cleanup = onCleanup(@() rmdir(work, 's'));
  filewrite(fullfile(work, 'data.txt'), 'RUNTIME_ARCHIVE_OK');
  archive = fullfile(work, 'input.zip');
  zip(archive, {'data.txt'}, [work, '/']);
  for version = 1:3
    executable = fullfile(work, ['fixture', int2str(version), '.exe']);
    make_fixture(executable, archive, version);
    __application_archive__('verify', executable);
    target = fullfile(work, ['out', int2str(version)]);
    __application_archive__('directory', target);
    __application_archive__('extract', executable, target);
    asserts.isequal(fileread(fullfile(target, 'data.txt')), 'RUNTIME_ARCHIVE_OK');
    asserts.isfalse(isfile(fullfile(target, '.nelson-payload.zip')));
    rejected(@() __application_archive__('extract', executable, target));
    fid = fopen(executable, 'r+b');
    fseek(fid, -320, 'eof');
    fwrite(fid, 'g', 'char');
    fclose(fid);
    rejected(@() __application_archive__('verify', executable));
  end
  forbidden = fullfile(work, 'forbidden.exe');
  for name = {'create', 'create-installed', 'create-external', 'create-external-installed'}
    args = {name{1}, fullfile(work, 'data.txt'), archive, forbidden, false};
    if endsWith(name{1}, 'installed'); args{end + 1} = repmat('a', 1, 64); end
    rejected(@() __application_archive__(args{:}));
    asserts.isfalse(isfile(forbidden));
    asserts.isfalse(isfile(fullfile(work, 'forbidden.nca')));
  end
  forbidden = fullfile(work, 'forbidden.zip');
  rejected(@() __application_archive__('pack', forbidden, {'data.txt'}, [work, '/']));
  asserts.isfalse(isfile(forbidden));
  for name = {'dotdot', 'absolute', 'drive', 'backslash'}
    archive = fullfile(modulepath('file_archiver', 'tests'), ['test_zipslip_', name{1}, '.zip']);
    executable = fullfile(work, [name{1}, '.exe']);
    make_fixture(executable, archive, 1);
    target = fullfile(work, name{1});
    __application_archive__('directory', target);
    rejected(@() __application_archive__('extract', executable, target));
    asserts.isfalse(isfile(fullfile(target, '.nelson-payload.zip')));
  end
  asserts.isfalse(isfile(fullfile(work, 'zipslip_canary.txt')));
  asserts.isequal(ismodule('compiler'), loaded);
  clear cleanup;
end

function make_fixture(executable, archive, version)
  % Fixed-format fixtures deliberately do not use the compiler's writer.
  stub = uint8('fixture-launcher');
  info = dir(archive);
  flags = 0;
  identifier = 'fixture.runtime';
  if version == 2; flags = 2; identifier = repmat('a', 1, 64); end
  if version == 3
    flags = 4;
    [folder, name] = fileparts(executable);
    copyfile(archive, fullfile(folder, [name, '.nca']));
  end
  input = fopen(archive, 'rb');
  closeInput = onCleanup(@() fclose(input));
  payload = fread(input, Inf, '*uint8');
  output = fopen(executable, 'wb', 'ieee-le');
  closeOutput = onCleanup(@() fclose(output));
  fwrite(output, stub, 'uint8');
  if version ~= 3; fwrite(output, payload, 'uint8'); end
  fwrite(output, 'NLSAPP01', 'char');
  fwrite(output, [version, flags], 'uint32');
  fwrite(output, [numel(stub), info.bytes], 'uint64');
  fwrite(output, sha256(archive, '-file'), 'char');
  fwrite(output, [uint8(identifier), zeros(1, 256 - numel(identifier), 'uint8')], 'uint8');
end

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end
