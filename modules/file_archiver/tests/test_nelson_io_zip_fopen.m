%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelson.io.zip.fopen: ZIP entries read and written with the usual file functions.
%=============================================================================
WORK = [tempdir(), createGUID()];
mkdir(WORK);
ARCHIVE = [WORK, '/streams.zip'];
hasZstd = any(nelson.io.zip.compressions() == "zstd");
%=============================================================================
% Writing entries through file identifiers.
z = nelson.io.zip.open(ARCHIVE, 'w');
fid = nelson.io.zip.fopen(z, 'table.csv', 'w');
asserts.istrue(fid > 2);
[name, permission] = fopen(fid);
asserts.isequal(name, [ARCHIVE, ':table.csv']);
asserts.isequal(permission, 'wb');
fprintf(fid, '%d,%g\n', [1:4; (1:4) / 4]);
asserts.isequal(fwrite(fid, 'end'), 3);
asserts.isequal(ftell(fid), 27);
% The archive waits for the entry to be finished.
asserts.checkerror('z.write(''other.txt'', ''x'')', ...
  'Close the file identifier of ZIP entry table.csv first.', 'Nelson:io:zip:entryOpen');
asserts.checkerror('z.fopen(''second.txt'', ''w'')', ...
  'Close the file identifier of ZIP entry table.csv first.', 'Nelson:io:zip:entryOpen');
asserts.checkerror('z.close()', 'Close the file identifier of ZIP entry table.csv first.', ...
  'Nelson:io:zip:entryOpen');
asserts.istrue(z.IsOpen);
asserts.isequal(fclose(fid), 0);
big = uint8(mod(0:299999, 251));
if hasZstd
  fid = z.fopen('big.bin', 'w', 'Compression', 'zstd');
else
  fid = z.fopen('big.bin', 'w');
end
for k = 1:3
  fwrite(fid, big((k - 1) * 100000 + 1:k * 100000));
end
fclose(fid);
fid = z.fopen('latin.txt', 'w', 'ISO-8859-1', 'Compression', 'store');
fprintf(fid, 'café');
fclose(fid);
asserts.checkerror('z.fopen(''big.bin'', ''w'')', 'The archive already has an entry named: big.bin', ...
  'Nelson:io:zip:duplicateEntry');
asserts.checkerror('z.fopen(''big.bin'', ''r'')', 'ZIP archive is open for writing.', ...
  'Nelson:io:zip:invalidState');
z.close();
%=============================================================================
% Reading entries through file identifiers.
r = nelson.io.zip.open(ARCHIVE);
T = r.entries();
asserts.isequal(T.Size(T.Name == "table.csv"), 27);
asserts.isequal(T.Size(T.Name == "big.bin"), 300000);
asserts.isequal(r.read('latin.txt'), uint8([99 97 102 233]));
fid = r.fopen('table.csv');
asserts.isequal(fgetl(fid), '1,0.25');
asserts.isequal(fgets(fid), sprintf('2,0.5\n'));
asserts.isequal(ftell(fid), 13);
C = textscan(fid, '%d,%f');
asserts.isequal(C{1}, int32([3; 4]));
asserts.isequal(C{2}, [0.75; 1]);
frewind(fid);
A = fscanf(fid, '%d,%f\n', [2 Inf]);
asserts.isequal(A, [1:4; (1:4) / 4]);
asserts.isequal(fgetl(fid), 'end');
asserts.isequal(fgetl(fid), -1);
asserts.isequal(feof(fid), 1);
[message, code] = ferror(fid);
asserts.isequal(code, 0);
fclose(fid);
fid = r.fopen('latin.txt', 'r', 'ISO-8859-1');
asserts.isequal(fread(fid, '*char')', 'café');
fclose(fid);
% Two readers of the same entry, positioning, size.
fid1 = r.fopen('big.bin');
fid2 = nelson.io.zip.fopen(r, 'big.bin', 'r');
asserts.isequal(fsize(fid1), 300000);
asserts.isequal(fseek(fid1, 200000, 'bof'), 0);
asserts.isequal(fread(fid1, 5, 'uint8=>uint8')', big(200001:200005));
asserts.isequal(fseek(fid1, 10, 'bof'), 0);
asserts.isequal(fread(fid1, 3, 'uint8=>uint8')', big(11:13));
asserts.isequal(fseek(fid1, -2, 'eof'), 0);
asserts.isequal(fread(fid1, Inf, 'uint8=>uint8')', big(end - 1:end));
asserts.isequal(fseek(fid1, 300001, 'bof'), -1);
asserts.isequal(fread(fid2, Inf, 'uint8=>uint8')', big);
fclose(fid1);
fclose(fid2);
% Limits and errors.
asserts.checkerror('r.fopen(''big.bin'', ''r'', ''MaxSize'', 1000)', 'ZIP entry exceeds MaxSize: big.bin', ...
  'Nelson:io:zip:tooLarge');
asserts.checkerror('r.fopen(''missing.txt'')', 'ZIP entry not found: missing.txt', ...
  'Nelson:io:zip:entryNotFound');
asserts.checkerror('r.fopen(''big.bin'', ''a'')', 'File mode must be ''r'' or ''w''.', ...
  'Nelson:io:invalidFileMode');
asserts.checkerror('r.fopen(''big.bin'', ''r'', ''no-such-encoding'')', 'Invalid encoding.', ...
  'Nelson:stream_manager:invalidEncodingArgument');
asserts.checkerror('r.fopen(''big.bin'', ''r'', ''Speed'', 1)', 'Unknown option: Speed', ...
  'Nelson:io:invalidOption');
r.close();
%=============================================================================
% A damaged entry fails its checksum when read through a file identifier.
m = nelson.io.zip.open([], 'w');
m.write('a.txt', repmat('abc', 1, 100), 'Compression', 'store');
bytes = m.tobytes();
data = find(bytes(1:end - 2) == 97 & bytes(2:end - 1) == 98 & bytes(3:end) == 99, 1);
bytes(data) = bitxor(bytes(data), uint8(1));
d = nelson.io.zip.open(bytes);
fid = d.fopen('a.txt');
[name, permission] = fopen(fid);
asserts.isequal(name, '<bytes>:a.txt');
try
  fread(fid, Inf, 'uint8');
  identifier = '';
catch e
  identifier = e.identifier;
end
asserts.isequal(identifier, 'Nelson:io:zip:checksumMismatch');
fclose(fid);
d.close();
%=============================================================================
% Deleting the archive object while an entry is written closes it when the entry ends.
z = nelson.io.zip.open([WORK, '/deferred.zip'], 'w');
fid = z.fopen('late.txt', 'w');
fwrite(fid, 'written after delete');
delete(z);
fclose(fid);
r = nelson.io.zip.open([WORK, '/deferred.zip']);
asserts.isequal(r.readtext('late.txt'), 'written after delete');
r.close();
% fclose('all') finishes an entry that is still open.
z = nelson.io.zip.open([WORK, '/all.zip'], 'w');
fid = z.fopen('one.txt', 'w');
fprintf(fid, 'one');
fclose('all');
z.close();
r = nelson.io.zip.open([WORK, '/all.zip']);
asserts.isequal(r.readtext('one.txt'), 'one');
r.close();
%=============================================================================
rmdir(WORK, 's');
%=============================================================================
