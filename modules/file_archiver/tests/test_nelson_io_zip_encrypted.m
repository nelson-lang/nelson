%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Archives made by 7-Zip with the password 'Secret': hello.txt and data.bin
% (100000 bytes, byte i = mod(31 i^2 + 7 i + 3, 256)).
%=============================================================================
TESTS = [modulepath('file_archiver', 'tests'), '/'];
k = 0:99999;
DATA = uint8(mod(31 * k .^ 2 + 7 * k + 3, 256));
HELLO = sprintf('Hello encrypted world!\n');
%=============================================================================
% WinZip AES-256 (deflate), AES-128 (stored) and traditional PKWARE encryption.
names = {'test_nelson_io_zip_aes256.zip', 'test_nelson_io_zip_aes128_store.zip', ...
  'test_nelson_io_zip_zipcrypto.zip'};
for n = 1:numel(names)
  ARCHIVE = [TESTS, names{n}];
  z = nelson.io.zip.open(ARCHIVE);
  T = nelson.io.zip.entries(z);
  asserts.isequal(T.Encrypted, [true; true]);
  asserts.istrue(all(ismember(T.Compression, ["store", "deflate"])));
  asserts.checkerror('nelson.io.zip.read(z, ''hello.txt'')', ...
    _('Encrypted entry needs a password (''Password'' option): hello.txt'), ...
    'Nelson:io:zip:encryptedEntry');
  nelson.io.zip.close(z);
  z = nelson.io.zip.open(ARCHIVE, 'r', 'Password', 'wrong');
  asserts.checkerror('nelson.io.zip.read(z, ''hello.txt'')', ...
    _('Wrong password for entry: hello.txt'), 'Nelson:io:zip:wrongPassword');
  nelson.io.zip.close(z);
  z = nelson.io.zip.open(ARCHIVE, 'r', 'Password', "Secret");
  asserts.isequal(nelson.io.zip.readtext(z, 'hello.txt'), HELLO);
  asserts.isequal(nelson.io.zip.read(z, 'data.bin'), DATA);
  fid = nelson.io.zip.fopen(z, 'data.bin');
  head = fread(fid, 1000, '*uint8')';
  rest = fread(fid, Inf, '*uint8')';
  fclose(fid);
  asserts.isequal([head, rest], DATA);
  nelson.io.zip.close(z);
end
%=============================================================================
% Plain and encrypted entries in one archive: the password is used only when needed.
z = nelson.io.zip.open([TESTS, 'test_nelson_io_zip_mixed_encrypted.zip'], 'r', 'Password', 'Secret');
T = nelson.io.zip.entries(z);
asserts.isequal(T.Encrypted(T.Name == "readme.txt"), false);
asserts.isequal(T.Encrypted(T.Name == "secret.txt"), true);
asserts.isequal(nelson.io.zip.readtext(z, 'readme.txt'), sprintf('plain entry\n'));
asserts.isequal(nelson.io.zip.readtext(z, 'secret.txt'), char([68 111 110 110 233 101 115 32 115 101 99 114 232 116 101 115 10]));
nelson.io.zip.close(z);
%=============================================================================
% Archive bytes in memory.
fid = fopen([TESTS, 'test_nelson_io_zip_aes256.zip'], 'r');
bytes = fread(fid, Inf, '*uint8')';
fclose(fid);
z = nelson.io.zip.open(bytes, 'r', 'Password', 'Secret');
asserts.isequal(nelson.io.zip.readtext(z, 'hello.txt'), HELLO);
nelson.io.zip.close(z);
%=============================================================================
% Option checks.
asserts.checkerror('nelson.io.zip.open(bytes, ''r'', ''Password'', 12)', ...
  'Password must be a character vector or a string scalar.', 'Nelson:io:zip:invalidTextOption');
asserts.checkerror('nelson.io.zip.open([], ''w'', ''Encryption'', ''aes128'')', ...
  'Encryption needs a Password and a write mode.', 'Nelson:io:zip:encryptionNeedsPassword');
asserts.checkerror('nelson.io.zip.open([], ''w'', ''Password'', ''x'', ''Encryption'', ''rot13'')', ...
  'Encryption must be ''aes256'', ''aes192'', ''aes128'' or ''zipcrypto''.', 'Nelson:io:zip:unknownEncryption');
%=============================================================================
% Writing encrypted entries; folders stay clear, reading needs the password.
WORK = [tempdir(), createGUID()];
mkdir([WORK, '/folder/sub']);
fid = fopen([WORK, '/folder/sub/data.txt'], 'w');
fprintf(fid, 'secret data');
fclose(fid);
% AES-192 is read but cannot be written.
asserts.checkerror('nelson.io.zip.open([], ''w'', ''Password'', ''x'', ''Encryption'', ''aes192'')', ...
  'AES-192 encryption cannot be written; use ''aes128'' or ''aes256''.', 'Nelson:io:zip:invalidArgument');
for encryption = {'aes256', 'aes128', 'zipcrypto'}
  for compression = {'deflate', 'store'}
    z = nelson.io.zip.open([], 'w', 'Password', 'pässword', 'Encryption', encryption{1}, 'Compression', compression{1});
    asserts.isequal(z.Encryption, string(encryption{1}));
    z.write('note.txt', HELLO);
    z.add([WORK, '/folder']);
    fid = z.fopen('stream.txt', 'w');
    fprintf(fid, 'streamed');
    fclose(fid);
    bytes = z.tobytes();
    r = nelson.io.zip.open(bytes);
    T = r.entries();
    asserts.isequal(T.Encrypted(T.Name == "note.txt"), true);
    asserts.isequal(T.Encrypted(T.Name == "folder/sub/data.txt"), true);
    asserts.checkerror('r.read(''note.txt'')', ...
      'Encrypted entry needs a password (''Password'' option): note.txt', 'Nelson:io:zip:encryptedEntry');
    r.close();
    r = nelson.io.zip.open(bytes, 'r', 'Password', 'pässword');
    asserts.isequal(r.readtext('note.txt'), HELLO);
    asserts.isequal(r.readtext('folder/sub/data.txt'), 'secret data');
    asserts.isequal(r.readtext('stream.txt'), 'streamed');
    r.close();
    r = nelson.io.zip.open(bytes, 'r', 'Password', 'wrong');
    asserts.checkerror('r.read(''note.txt'')', 'Wrong password for entry: note.txt', 'Nelson:io:zip:wrongPassword');
    r.close();
  end
end
rmdir(WORK, 's');
%=============================================================================
