%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
WORK = [tempdir(), createGUID()];
%=============================================================================
asserts.istrue(nelson.io.gzip.isavailable());
asserts.istrue(strlength(nelson.io.gzip.version()) > 0);
% Bytes written by another gzip implementation.
external = uint8([31, 139, 8, 0, 0, 0, 0, 0, 2, 255, 203, 72, 205, 201, 201, 87, 72, 175, 202, 44, 0, 0, 25, 106, 210, 223, 10, 0, 0, 0]);
asserts.isequal(char(nelson.io.gzip.decompress(external)), 'hello gzip');
asserts.isequal(nelson.io.gzip.decompresstext(external), 'hello gzip');
% Round trips.
data = uint8(mod(1:100000, 7));
packed = nelson.io.gzip.compress(data);
asserts.isequal(packed(1:2), uint8([31 139]));
asserts.isequal(nelson.io.gzip.decompress(packed), data);
asserts.istrue(numel(nelson.io.gzip.compress(data, 'Level', 9)) <= numel(nelson.io.gzip.compress(data, 'Level', 1)));
asserts.isequal(nelson.io.gzip.decompresstext(nelson.io.gzip.compress("été")), 'été');
asserts.isequal(nelson.io.gzip.decompress(nelson.io.gzip.compress(uint8([]))), uint8(zeros(1, 0)));
% Concatenated members.
asserts.isequal(char(nelson.io.gzip.decompress([nelson.io.gzip.compress('ab'), nelson.io.gzip.compress('cd')])), 'abcd');
% Errors.
asserts.checkerror('nelson.io.gzip.compress(''x'', ''Level'', 10)', 'Level must be an integer from 0 to 9.', 'Nelson:io:gzip:invalidLevel');
asserts.checkerror('nelson.io.gzip.decompress(packed, ''MaxSize'', 10)', 'Decompressed data exceeds MaxSize.', 'Nelson:io:gzip:tooLarge');
try
  nelson.io.gzip.decompress(packed(1:end - 5));
  identifier = '';
catch e
  identifier = e.identifier;
end
asserts.isequal(identifier, 'Nelson:io:gzip:invalidData');
asserts.checkerror('nelson.io.gzip.decompress(''abc'')', 'Data must be uint8.', 'Nelson:io:invalidData');
%=============================================================================
mkdir(WORK);
SOURCE = [WORK, '/data.txt'];
fid = fopen(SOURCE, 'w');
fprintf(fid, 'line %d\n', 1:500);
fclose(fid);
expected = fileread(SOURCE);
GZ = nelson.io.gzip.compressfile(SOURCE);
asserts.isequal(GZ, [SOURCE, '.gz']);
delete(SOURCE);
asserts.isequal(nelson.io.gzip.decompressfile(GZ), SOURCE);
asserts.isequal(fileread(SOURCE), expected);
OTHER = nelson.io.gzip.decompressfile(GZ, [WORK, '/other.txt']);
asserts.isequal(fileread(OTHER), expected);
asserts.checkerror('nelson.io.gzip.decompressfile(SOURCE)', ...
  'A destination is needed when the source does not end with .gz.', 'Nelson:io:gzip:destinationExpected');
%=============================================================================
% fopen: read, write, append, in memory.
fid = nelson.io.gzip.fopen(GZ);
asserts.isequal(fgetl(fid), 'line 1');
fseek(fid, -7, 'eof');
asserts.isequal(fgetl(fid), 'ne 500');
frewind(fid);
values = textscan(fid, '%s %d');
asserts.isequal(values{2}(end), int32(500));
fclose(fid);
WRITTEN = [WORK, '/written.gz'];
fid = nelson.io.gzip.fopen(WRITTEN, 'w', 'Level', 9);
fwrite(fid, uint8(1:10));
fprintf(fid, 'end');
fclose(fid);
fid = nelson.io.gzip.fopen(WRITTEN, 'a');
fprintf(fid, '!');
fclose(fid);
raw = fopen(WRITTEN);
bytes = fread(raw, Inf, 'uint8=>uint8')';
fclose(raw);
asserts.isequal(nelson.io.gzip.decompress(bytes), [uint8(1:10), uint8('end!')]);
fid = nelson.io.gzip.fopen([], 'w');
fprintf(fid, 'memory');
bytes = nelson.io.gzip.tobytes(fid);
fid = nelson.io.gzip.fopen(bytes);
value = fread(fid, Inf, 'char=>char');
asserts.isequal(value(:)', 'memory');
fclose(fid);
asserts.checkerror('nelson.io.gzip.fopen([], ''a'')', 'Appending needs a file name.', 'Nelson:io:gzip:appendNeedsFile');
NOT_GZIP = [WORK, '/plain.txt'];
raw = fopen(NOT_GZIP, 'w');
fprintf(raw, 'plain');
fclose(raw);
try
  nelson.io.gzip.fopen(NOT_GZIP, 'a');
  identifier = '';
catch e
  identifier = e.identifier;
end
asserts.isequal(identifier, 'Nelson:io:gzip:invalidData');
%=============================================================================
rmdir(WORK, 's');
%=============================================================================
