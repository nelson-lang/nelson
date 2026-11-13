%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelson.io.zip: read and write ZIP entries without extracting files.
%=============================================================================
WORK = [tempdir(), createGUID()];
mkdir(WORK);
ARCHIVE = [WORK, '/a.zip'];
methods = nelson.io.zip.compressions();
asserts.istrue(isstring(methods));
asserts.istrue(all(ismember(["store", "deflate"], methods)));
hasZstd = any(methods == "zstd");
%=============================================================================
% Writing text, bytes and stored entries; the level applies to the whole archive.
z = nelson.io.zip.open(ARCHIVE, 'w', 'Level', 9);
asserts.istrue(isa(z, 'nelson.io.ZipArchive'));
asserts.istrue(z.IsOpen);
nelson.io.zip.write(z, 'text/hello.txt', 'héllo');
z.write('data.bin', uint8(0:255));
z.write('raw.txt', "stored", 'Compression', 'store');
if hasZstd
  z.write('packed.bin', uint8(repmat(1:10, 1, 100)), 'Compression', 'zstd', 'Level', 9);
end
asserts.checkerror('z.write(''other.txt'', ''x'', ''Level'', 1)', ...
  'All entries of a ZIP archive use the compression level of its first file.', 'Nelson:io:zip:invalidArgument');
when = datetime(2021, 6, 7, 8, 9, 10);
z.write('dated.txt', 'old', 'Modified', when);
nelson.io.zip.close(z);
asserts.isfalse(z.IsOpen);
%=============================================================================
% Reading back.
r = nelson.io.zip.open(ARCHIVE);
T = nelson.io.zip.entries(r);
asserts.isequal(T.Properties.VariableNames, ...
  {'Name', 'Size', 'CompressedSize', 'Compression', 'Modified', 'IsFolder', 'CRC32', 'Encrypted'});
asserts.istrue(any(T.Name == "text/hello.txt"));
asserts.isequal(T.Compression(T.Name == "raw.txt"), "store");
asserts.isequal(T.Compression(T.Name == "data.bin"), "deflate");
asserts.istrue(abs(seconds(T.Modified(T.Name == "dated.txt") - when)) <= 2);
asserts.isequal(nelson.io.zip.readtext(r, 'text/hello.txt'), 'héllo');
asserts.isequal(nelson.io.zip.read(r, 'data.bin'), uint8(0:255));
asserts.isequal(r.readtext('raw.txt'), 'stored');
if hasZstd
  asserts.isequal(T.Compression(T.Name == "packed.bin"), "zstd");
  asserts.isequal(r.read('packed.bin'), uint8(repmat(1:10, 1, 100)));
end
asserts.isequal(r.read('data.bin', 'MaxSize', 256), uint8(0:255));
asserts.checkerror('r.read(''data.bin'', ''MaxSize'', 255)', 'ZIP entry exceeds MaxSize: data.bin', ...
  'Nelson:io:zip:tooLarge');
asserts.checkerror('r.readtext(''raw.txt'', ''MaxSize'', 2)', 'ZIP entry exceeds MaxSize: raw.txt', ...
  'Nelson:io:zip:tooLarge');
asserts.isequal(r.readtext('raw.txt', 'MaxSize', 6, 'Encoding', 'UTF-8'), 'stored');
asserts.checkerror('r.read(''data.bin'', ''MaxSize'', 1.5)', 'MaxSize must be Inf or a nonnegative integer.', ...
  'Nelson:io:invalidMaxSize');
asserts.checkerror('r.read(''missing.txt'')', 'ZIP entry not found: missing.txt', 'Nelson:io:zip:entryNotFound');
r.close();
asserts.checkerror('r.read(''data.bin'')', 'ZIP archive is closed.', 'Nelson:io:zip:invalidState');
%=============================================================================
% unzip reads what nelson.io.zip wrote.
R = unzip(ARCHIVE, [WORK, '/out']);
asserts.istrue(isfile([WORK, '/out/text/hello.txt']));
asserts.isequal(fileread([WORK, '/out/raw.txt']), 'stored');
%=============================================================================
% In-memory archives.
m = nelson.io.zip.open([], 'w');
m.write('x.txt', 'in memory');
bytes = nelson.io.zip.tobytes(m);
asserts.istrue(isa(bytes, 'uint8'));
asserts.isfalse(m.IsOpen);
m2 = nelson.io.zip.open(bytes);
asserts.isequal(m2.readtext('x.txt'), 'in memory');
asserts.isequal(m2.tobytes(), bytes);
m2.close();
%=============================================================================
% Adding files and folders from disk.
SRC = [WORK, '/src'];
mkdir([SRC, '/empty']);
mkdir([SRC, '/sub']);
filewrite([SRC, '/a.txt'], 'a');
filewrite([SRC, '/sub/b.txt'], 'b');
z = nelson.io.zip.open([WORK, '/added.zip'], 'x');
names = z.add([SRC, '/a.txt']);
asserts.isequal(names, "a.txt");
names = nelson.io.zip.add(z, SRC, 'tree');
asserts.isequal(sort(names), sort(["tree/a.txt"; "tree/empty/"; "tree/sub/b.txt"]));
z.close();
asserts.checkerror('nelson.io.zip.open([WORK, ''/added.zip''], ''x'')', ...
  ['Cannot open ZIP archive: ', WORK, '/added.zip'], 'Nelson:io:zip:cannotOpen');
%=============================================================================
% Appending, renaming and removing.
z = nelson.io.zip.open([WORK, '/added.zip'], 'a');
asserts.isequal(numel(z.entries().Name), 4);
z.write('appended.txt', 'more');
asserts.checkerror('z.write(''a.txt'', ''again'')', 'The archive already has an entry named: a.txt', ...
  'Nelson:io:zip:duplicateEntry');
asserts.checkerror('z.add([SRC, ''/a.txt''])', 'The archive already has an entry named: a.txt', ...
  'Nelson:io:zip:duplicateEntry');
z.close();
nelson.io.zip.rename([WORK, '/added.zip'], 'a.txt', 'renamed.txt');
nelson.io.zip.remove([WORK, '/added.zip'], ["tree/empty/", "appended.txt"]);
r = nelson.io.zip.open([WORK, '/added.zip']);
asserts.isequal(sort(r.entries().Name), sort(["renamed.txt"; "tree/a.txt"; "tree/sub/b.txt"]));
asserts.isequal(r.readtext('renamed.txt'), 'a');
r.close();
asserts.checkerror('nelson.io.zip.remove([WORK, ''/added.zip''], ''nope'')', ...
  'ZIP entry not found: nope', 'Nelson:io:zip:entryNotFound');
%=============================================================================
% A folder name renames or removes every entry under it.
nelson.io.zip.rename([WORK, '/added.zip'], 'tree', 'forest');
r = nelson.io.zip.open([WORK, '/added.zip']);
asserts.isequal(sort(r.entries().Name), sort(["renamed.txt"; "forest/a.txt"; "forest/sub/b.txt"]));
asserts.isequal(r.readtext('forest/sub/b.txt'), 'b');
r.close();
nelson.io.zip.rename([WORK, '/added.zip'], 'forest/sub/', 'leaves/');
asserts.checkerror('nelson.io.zip.rename([WORK, ''/added.zip''], ''renamed.txt'', ''leaves/b.txt'')', ...
  'The archive already has an entry named: leaves/b.txt', 'Nelson:io:zip:duplicateEntry');
nelson.io.zip.remove([WORK, '/added.zip'], 'forest/');
r = nelson.io.zip.open([WORK, '/added.zip']);
asserts.isequal(sort(r.entries().Name), sort(["renamed.txt"; "leaves/b.txt"]));
%=============================================================================
% The object shows its properties.
shown = evalc('disp(r)');
asserts.istrue(contains(shown, 'Mode: "r"'));
asserts.istrue(contains(shown, 'IsOpen: true'));
r.close();
%=============================================================================
% Option checks.
z = nelson.io.zip.open([], 'w');
asserts.checkerror('z.write(''a'', ''x'', ''Compression'', ''ppmd'')', ...
  'Compression must be ''store'', ''deflate'', ''zstd'', ''xz'', ''bzip2'' or ''lzma'': ppmd', ...
  'Nelson:io:zip:unknownCompression');
asserts.checkerror('z.write(''a'', ''x'', ''Level'', 10)', 'Level must be an integer from 0 to 9.', ...
  'Nelson:io:zip:invalidLevel');
z.write('a', 'x');
asserts.checkerror('z.write(''a'', ''y'')', 'The archive already has an entry named: a', ...
  'Nelson:io:zip:duplicateEntry');
asserts.checkerror('z.write(''b'', {1})', 'Data must be uint8, char or a string scalar.', ...
  'Nelson:io:invalidData');
asserts.checkerror('z.write(''b'', ''x'', ''Speed'', 1)', 'Unknown option: Speed', 'Nelson:io:invalidOption');
z.close();
asserts.checkerror('nelson.io.zip.open(uint8([1 2 3]))', 'Not a valid ZIP archive: <bytes>', ...
  'Nelson:io:zip:notZipArchive');
%=============================================================================
cd(tempdir());
rmdir(WORK, 's');
%=============================================================================
