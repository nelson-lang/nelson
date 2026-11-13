%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unzip with Zstandard and bzip2 entries, and with an unsupported compression method.
%=============================================================================
TESTS = [nelsonroot(), '/modules/file_archiver/tests/'];
WORK = [tempdir(), createGUID()];
mkdir(WORK);
%=============================================================================
% Zstandard (method 93) is decoded when Nelson is built with libzstd.
DEST = [WORK, '/zstd'];
errorId = '';
try
  R = unzip([TESTS, 'test_unzip_zstd.zip'], DEST);
catch e
  errorId = e.identifier;
  errorMessage = e.message;
end
if isempty(errorId)
  asserts.isequal(R, {[DEST, '/zstd/hello.txt'], [DEST, '/zstd/plain.txt']});
  asserts.isequal(fileread(R{1}), repmat(sprintf('hello zstd\n'), 1, 64));
  asserts.isequal(fileread(R{2}), repmat(sprintf('deflate\n'), 1, 64));
else
  asserts.isequal(errorId, 'Nelson:unzip:unsupportedCompressionMethod');
  asserts.istrue(contains(errorMessage, '93'));
  asserts.istrue(contains(errorMessage, 'zstd/hello.txt'));
end
%=============================================================================
% bzip2 (method 12) is decoded.
DEST = [WORK, '/bzip2'];
R = unzip([TESTS, 'test_unzip_bzip2.zip'], DEST);
asserts.isequal(fileread(R{1}), repmat(sprintf('bzip2\n'), 1, 64));
%=============================================================================
% Deflate64 (method 9) is not supported: the error names the method and the entry.
z = nelson.io.zip.open([], 'w', 'Compression', 'store');
z.write('packed.txt', 'stored text');
bytes = z.tobytes();
signature = @(b, c, d) find(bytes(1:end - 3) == 80 & bytes(2:end - 2) == 75 & bytes(3:end - 1) == c & bytes(4:end) == d);
local = signature(bytes, 3, 4);
central = signature(bytes, 1, 2);
bytes(local(1) + 8) = 9;
bytes(central(1) + 10) = 9;
patched = [WORK, '/deflate64.zip'];
fid = fopen(patched, 'wb');
fwrite(fid, bytes);
fclose(fid);
DEST = [WORK, '/deflate64'];
msg = sprintf(_('Unsupported compression method %d for entry: %s'), 9, 'packed.txt');
asserts.checkerror('unzip(patched, DEST)', msg, 'Nelson:unzip:unsupportedCompressionMethod');
asserts.isfalse(isfile([DEST, '/packed.txt']));
%=============================================================================
cd(tempdir());
rmdir(WORK, 's');
%=============================================================================
