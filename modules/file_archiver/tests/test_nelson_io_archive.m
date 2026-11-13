%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelson.io.archive reads 7z, cpio, iso and other formats; encrypted entries need a password.
TESTS = [nelsonroot(), '/modules/file_archiver/tests/'];
WORK = [tempdir(), createGUID()];
mkdir(WORK);
cleanup = onCleanup(@() rmdir(WORK, 's'));
%=============================================================================
fixtures = {'test_archive_sample.7z', 'test_archive_sample.cpio', 'test_archive_sample.iso.gz'};
for k = 1:numel(fixtures)
  a = nelson.io.archive.open([TESTS, fixtures{k}]);
  asserts.istrue(isa(a, 'nelson.io.Archive'));
  asserts.istrue(strlength(a.Format) > 0);
  T = a.entries();
  asserts.istrue(any(T.Name == "a.txt"));
  asserts.istrue(any(T.Name == "dir/b.txt"));
  asserts.isequal(a.readtext('a.txt'), 'archive text');
  asserts.isequal(char(a.read('dir/b.txt')), 'second file');
  fid = a.fopen('dir/b.txt');
  asserts.isequal(fread(fid, '*char')', 'second file');
  fclose(fid);
  out = fullfile(WORK, sprintf('out%d', k));
  names = a.extract(out);
  asserts.istrue(any(endsWith(names, 'a.txt')));
  asserts.isequal(fileread(fullfile(out, 'dir', 'b.txt')), 'second file');
  a.close();
  asserts.isfalse(a.IsOpen);
end
asserts.isequal(nelson.io.archive.open([TESTS, 'test_archive_sample.iso.gz']).Compression, "gzip");
%=============================================================================
% Bytes in memory and one-shot extraction with limits.
fid = fopen([TESTS, 'test_archive_sample.7z'], 'rb');
bytes = fread(fid, Inf, '*uint8')';
fclose(fid);
a = nelson.io.archive.open(bytes);
asserts.isequal(a.readtext('a.txt'), 'archive text');
a.close();
out = fullfile(WORK, 'oneshot');
names = nelson.io.archive.extract([TESTS, 'test_archive_sample.7z'], out);
asserts.isequal(fileread(fullfile(out, 'a.txt')), 'archive text');
SAMPLE = [TESTS, 'test_archive_sample.7z'];
SMALL = fullfile(WORK, 'small');
msg = sprintf('Extracting %s needs 23 bytes, more than MaxExtractedSize (5 bytes).', SAMPLE);
asserts.checkerror('nelson.io.archive.extract(SAMPLE, SMALL, ''MaxExtractedSize'', 5)', msg, ...
  'Nelson:file_archiver:extractedSizeLimit');
%=============================================================================
% Encrypted 7z: names are listed, the data cannot be decrypted.
encrypted = [TESTS, 'test_archive_encrypted.7z'];
a = nelson.io.archive.open(encrypted);
T = a.entries();
asserts.istrue(all(T.Encrypted(~T.IsFolder)));
asserts.checkerror('a.read(''a.txt'')', ...
  'Encrypted entry needs a password (''Password'' option): a.txt', 'Nelson:io:archive:encryptedEntry');
a.close();
a = nelson.io.archive.open(encrypted, 'Password', 'secret');
asserts.checkerror('a.read(''a.txt'')', ...
  'The encryption of this entry is not supported: a.txt', 'Nelson:io:archive:unsupportedEncryption');
a.close();
% Encrypted ZIP entries are decrypted with the password.
z = nelson.io.zip.open([], 'w', 'Password', 'secret', 'Encryption', 'aes256');
z.write('dir/c.txt', 'zip secret');
zipped = z.tobytes();
a = nelson.io.archive.open(zipped, 'Password', 'secret');
asserts.isequal(a.readtext('dir/c.txt'), 'zip secret');
a.close();
a = nelson.io.archive.open(zipped, 'Password', 'wrong');
asserts.checkerror('a.read(''dir/c.txt'')', 'Wrong password for entry: dir/c.txt', ...
  'Nelson:io:archive:wrongPassword');
a.close();
%=============================================================================
% Errors.
asserts.checkerror('nelson.io.archive.open(uint8(1:100))', 'Not a supported archive: <bytes>', ...
  'Nelson:io:archive:unknownFormat');
asserts.checkerror('nelson.io.archive.open([])', 'Source must be a file name or uint8 data.', ...
  'Nelson:io:archive:invalidSource');
%=============================================================================
% tar symbolic links are restored inside the destination only (not on Windows).
if ~ispc()
  src = fullfile(WORK, 'links');
  mkdir(src);
  filewrite(fullfile(src, 'target.txt'), 'target');
  system(['cd "', src, '" && ln -s target.txt inside.lnk && ln -s ../../outside.txt outside.lnk']);
  tarfile = fullfile(WORK, 'links.tar');
  system(['cd "', src, '" && tar -cf "', tarfile, '" target.txt inside.lnk outside.lnk']);
  out = fullfile(WORK, 'links_out');
  names = untar(tarfile, out);
  asserts.isequal(fileread(fullfile(out, 'inside.lnk')), 'target');
  asserts.isfalse(any(endsWith(names, 'outside.lnk')));
  [status, ~] = system(['test -L "', fullfile(out, 'outside.lnk'), '"']);
  asserts.isfalse(status == 0);
end
%=============================================================================
