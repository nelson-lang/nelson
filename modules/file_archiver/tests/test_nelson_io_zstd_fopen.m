%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelson.io.zstd.fopen: Zstandard data read and written with the usual file functions.
%=============================================================================
if ~nelson.io.zstd.isavailable()
  asserts.checkerror('nelson.io.zstd.fopen([], ''w'')', 'Zstandard is not available in this build.', ...
    'Nelson:io:zstd:notAvailable');
  return
end
WORK = [tempdir(), createGUID()];
mkdir(WORK);
ZST = [WORK, '/lines.txt.zst'];
%=============================================================================
% Writing a .zst file line by line.
fid = nelson.io.zstd.fopen(ZST, 'w', 'Level', 5);
[name, permission] = fopen(fid);
asserts.isequal(name, ZST);
asserts.isequal(permission, 'wb');
for k = 1:2000
  fprintf(fid, 'line %d\n', k);
end
% The destination appears only when the writer is closed.
asserts.isfalse(isfile(ZST));
fclose(fid);
asserts.istrue(isfile(ZST));
expected = sprintf('line %d\n', 1:2000);
raw = fopen(ZST);
packed = fread(raw, Inf, 'uint8=>uint8')';
fclose(raw);
asserts.isequal(char(nelson.io.zstd.decompress(packed)), expected);
%=============================================================================
% Reading it back.
fid = nelson.io.zstd.fopen(ZST);
count = 0;
line = fgetl(fid);
while ischar(line)
  count = count + 1;
  line = fgetl(fid);
end
asserts.isequal(count, 2000);
asserts.isequal(feof(fid), 1);
asserts.isequal(fsize(fid), numel(expected));
frewind(fid);
C = textscan(fid, '%s %d');
asserts.isequal(C{2}, int32((1:2000)'));
frewind(fid);
asserts.isequal(fscanf(fid, '%s', 1), 'line');
asserts.isequal(fscanf(fid, '%d', 1), 1);
asserts.isequal(fseek(fid, 7, 'bof'), 0);
asserts.isequal(fgetl(fid), 'line 2');
fclose(fid);
%=============================================================================
% In memory: writer to bytes, bytes to reader.
fid = nelson.io.zstd.fopen([], 'w');
fwrite(fid, 'héllo');
fwrite(fid, uint8([10 0 255]));
bytes = nelson.io.zstd.tobytes(fid);
asserts.isfalse(any(fopen('all') == fid));
asserts.isequal(nelson.io.zstd.decompress(bytes), [uint8('h'), uint8([195 169]), uint8('llo'), uint8([10 0 255])]);
fid = nelson.io.zstd.fopen(bytes, 'r');
asserts.isequal(fopen(fid), '<bytes>');
asserts.isequal(fgetl(fid), 'héllo');
asserts.isequal(fread(fid, Inf, 'uint8=>uint8')', uint8([0 255]));
fclose(fid);
fid = nelson.io.zstd.fopen(nelson.io.zstd.compress(unicode2native('café', 'ISO-8859-1')), 'r', 'ISO-8859-1');
asserts.isequal(fread(fid, '*char')', 'café');
fclose(fid);
%=============================================================================
% Limits and errors.
fid = nelson.io.zstd.fopen(ZST, 'r', 'MaxSize', 100);
try
  fread(fid, Inf, 'uint8');
  identifier = '';
catch e
  identifier = e.identifier;
end
asserts.isequal(identifier, 'Nelson:io:zstd:tooLarge');
fclose(fid);
truncated = bytes(1:end - 3);
fid = nelson.io.zstd.fopen(truncated);
try
  fread(fid, Inf, 'uint8');
  identifier = '';
catch e
  identifier = e.identifier;
end
asserts.isequal(identifier, 'Nelson:io:zstd:invalidData');
fclose(fid);
asserts.checkerror('nelson.io.zstd.fopen([WORK, ''/missing.zst''])', ...
  ['Cannot read file: ', WORK, '/missing.zst'], 'Nelson:io:zstd:cannotRead');
asserts.checkerror('nelson.io.zstd.fopen(uint8([1 2]), ''w'')', ...
  'Source must be a file name, uint8 data to read, or [] to write in memory.', ...
  'Nelson:io:zstd:invalidSource');
asserts.checkerror('nelson.io.zstd.fopen(ZST, ''r+'')', 'File mode must be ''r'', ''w'' or ''a''.', ...
  'Nelson:io:invalidAppendFileMode');
fid = nelson.io.zstd.fopen(ZST);
asserts.checkerror('nelson.io.zstd.tobytes(fid)', ...
  'The file identifier must be an in-memory writer opened with fopen([], ''w'').', ...
  'Nelson:io:notMemoryWriter');
fclose(fid);
% An unfinished writer closed by fclose('all') still produces a valid file.
fid = nelson.io.zstd.fopen([WORK, '/all.zst'], 'w');
fprintf(fid, 'kept');
fclose('all');
raw = fopen([WORK, '/all.zst']);
packed = fread(raw, Inf, 'uint8=>uint8')';
fclose(raw);
asserts.isequal(char(nelson.io.zstd.decompress(packed)), 'kept');
% Backward jumps over several blocks: the bytes come back from the cache.
big = uint8(mod((1:300000) * 7, 251));
fid = nelson.io.zstd.fopen([WORK, '/big.zst'], 'w');
fwrite(fid, big);
fclose(fid);
fid = nelson.io.zstd.fopen([WORK, '/big.zst']);
positions = [250000, 10, 140000, 0, 299990, 70000, 5];
for p = positions
  asserts.isequal(fseek(fid, p, 'bof'), 0);
  asserts.isequal(ftell(fid), p);
  asserts.isequal(fread(fid, 10, 'uint8=>uint8')', big(p + 1:p + 10));
end
asserts.isequal(fseek(fid, 300000, 'bof'), 0);
asserts.istrue(isempty(fread(fid, 1, 'uint8')));
asserts.isequal(feof(fid), 1);
frewind(fid);
asserts.isequal(fread(fid, Inf, 'uint8=>uint8')', big);
fclose(fid);
% fscanf reads again after a backward jump.
lines = sprintf('%d
', 1:20000);
fid = nelson.io.zstd.fopen([WORK, '/numbers.zst'], 'w');
fprintf(fid, '%s', lines);
fclose(fid);
fid = nelson.io.zstd.fopen([WORK, '/numbers.zst']);
asserts.isequal(fscanf(fid, '%d', 3)', [1 2 3]);
fseek(fid, 0, 'eof');
frewind(fid);
values = fscanf(fid, '%d');
asserts.isequal(values', 1:20000);
asserts.isequal(feof(fid), 1);
frewind(fid);
asserts.isequal(fgetl(fid), '1');
fclose(fid);
% Mode 'a' adds a frame: the file reads back as the concatenation.
APPENDED = [WORK, '/appended.zst'];
fid = nelson.io.zstd.fopen(APPENDED, 'a');
fprintf(fid, 'first;');
fclose(fid);
fid = nelson.io.zstd.fopen(APPENDED, 'a', 'Level', 9);
asserts.isequal(ftell(fid), 0);
fprintf(fid, 'second');
fclose(fid);
raw = fopen(APPENDED);
packed = fread(raw, Inf, 'uint8=>uint8')';
fclose(raw);
asserts.isequal(char(nelson.io.zstd.decompress(packed)), 'first;second');
fid = nelson.io.zstd.fopen(APPENDED);
asserts.isequal(fgetl(fid), 'first;second');
fclose(fid);
NOT_ZSTD = [WORK, '/plain.txt'];
raw = fopen(NOT_ZSTD, 'w');
fprintf(raw, 'plain text');
fclose(raw);
try
  nelson.io.zstd.fopen(NOT_ZSTD, 'a');
  identifier = '';
catch e
  identifier = e.identifier;
end
asserts.isequal(identifier, 'Nelson:io:zstd:invalidData');
asserts.checkerror('nelson.io.zstd.fopen([], ''a'')', 'Appending needs a file name.', ...
  'Nelson:io:zstd:appendNeedsFile');
% Seekable format: 1 MiB frames and a seek table; jumps decode one frame.
SEEKABLE = [WORK, '/seekable.zst'];
sdata = uint8(mod((1:2600000) * 7, 251));
fid = nelson.io.zstd.fopen(SEEKABLE, 'w', 'Seekable', true);
fwrite(fid, sdata);
fclose(fid);
raw = fopen(SEEKABLE);
packed = fread(raw, Inf, 'uint8=>uint8')';
fclose(raw);
asserts.isequal(nelson.io.zstd.decompress(packed), sdata);
for source = {SEEKABLE, packed}
  fid = nelson.io.zstd.fopen(source{1});
  fseek(fid, 0, 'eof');
  asserts.isequal(ftell(fid), 2600000);
  for p = [2500000, 3, 1048575, 1048576, 2097160, 0]
    asserts.isequal(fseek(fid, p, 'bof'), 0);
    asserts.isequal(fread(fid, 8, 'uint8=>uint8')', sdata(p + 1:p + 8));
  end
  fclose(fid);
end
fid = nelson.io.zstd.fopen([WORK, '/empty_seekable.zst'], 'w', 'Seekable', true);
fclose(fid);
fid = nelson.io.zstd.fopen([WORK, '/empty_seekable.zst']);
asserts.istrue(isempty(fread(fid, Inf, 'uint8')));
fclose(fid);
asserts.checkerror('nelson.io.zstd.fopen(SEEKABLE, ''a'', ''Seekable'', true)', ...
  'Seekable data cannot be appended.', 'Nelson:io:zstd:seekableAppend');
asserts.checkerror('nelson.io.zstd.compress(uint8(1:4), ''Workers'', -2)', ...
  'Workers must be ''auto'' or an integer from 0 to 256.', 'Nelson:io:zstd:invalidWorkers');
asserts.isequal(nelson.io.zstd.decompress(nelson.io.zstd.compress(sdata, 'Workers', 4)), sdata);
%=============================================================================
rmdir(WORK, 's');
%=============================================================================
