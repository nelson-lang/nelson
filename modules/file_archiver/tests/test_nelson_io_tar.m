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
mkdir(WORK);
mkdir([WORK, '/src/sub']);
fid = fopen([WORK, '/src/a.txt'], 'w');
fprintf(fid, 'alpha\n');
fclose(fid);
fid = fopen([WORK, '/src/sub/b.bin'], 'w');
fwrite(fid, uint8(0:255));
fclose(fid);
mkdir([WORK, '/src/empty']);
compressions = cellstr(nelson.io.tar.compressions());
asserts.istrue(any(strcmp(compressions, 'gzip')));
%=============================================================================
for c = compressions'
  compression = c{1};
  extensions = struct('none', '.tar', 'gzip', '.tar.gz', 'zstd', '.tar.zst', 'xz', '.tar.xz', ...
    'bzip2', '.tar.bz2', 'lz4', '.tar.lz4', 'lzma', '.tar.lzma');
  ext = extensions.(compression);
  TARFILE = [WORK, '/archive', ext];
  t = nelson.io.tar.open(TARFILE, 'w');
  asserts.isequal(t.Compression, string(compression));
  added = t.add([WORK, '/src']);
  asserts.istrue(any(added == "src/a.txt"));
  asserts.istrue(any(added == "src/empty/"));
  t.write('notes/été.txt', "texte accentué", 'Modified', datetime(2024, 5, 6, 7, 8, 9));
  t.write(repmat('long/', 1, 30) + "name.txt", 'long');
  fid = t.fopen('stream.txt', 'w');
  fprintf(fid, 'line %d\n', 1:100);
  asserts.checkerror('t.close()', 'Close the file identifier of tar entry stream.txt first.', 'Nelson:io:tar:entryOpen');
  fclose(fid);
  asserts.checkerror('t.write(''notes/été.txt'', ''x'')', 'The archive already has an entry named: notes/été.txt', 'Nelson:io:tar:duplicateEntry');
  t.close();
  asserts.isfalse(t.IsOpen);
  r = nelson.io.tar.open(TARFILE);
  asserts.isequal(r.Compression, string(compression));
  T = r.entries();
  asserts.istrue(any(T.Name == "src/sub/b.bin"));
  asserts.isequal(T.Size(T.Name == "src/sub/b.bin"), 256);
  asserts.istrue(T.IsFolder(T.Name == "src/empty/"));
  asserts.isequal(T.Modified(T.Name == "notes/été.txt"), datetime(2024, 5, 6, 7, 8, 9));
  asserts.isequal(r.read('src/sub/b.bin'), uint8(0:255));
  asserts.isequal(r.readtext('notes/été.txt'), 'texte accentué');
  asserts.isequal(r.readtext(repmat('long/', 1, 30) + "name.txt"), 'long');
  asserts.isequal(r.readtext('src/a.txt'), sprintf('alpha\n'));
  fid = r.fopen('stream.txt');
  asserts.isequal(fgetl(fid), 'line 1');
  values = textscan(fid, '%s %d');
  asserts.isequal(values{2}(end), int32(100));
  frewind(fid);
  asserts.isequal(fgetl(fid), 'line 1');
  fclose(fid);
  asserts.checkerror('r.read(''missing.txt'')', 'tar entry not found: missing.txt', 'Nelson:io:tar:entryNotFound');
  asserts.checkerror('r.read(''src/sub/b.bin'', ''MaxSize'', 10)', 'tar entry exceeds MaxSize: src/sub/b.bin', 'Nelson:io:tar:tooLarge');
  OUT = [WORK, '/out_', compression];
  files = r.extract(OUT);
  asserts.istrue(isfile([OUT, '/src/sub/b.bin']));
  asserts.istrue(isfolder([OUT, '/src/empty']));
  asserts.isequal(fileread([OUT, '/notes/été.txt']), 'texte accentué');
  asserts.istrue(numel(files) >= 6);
  r.close();
end
%=============================================================================
% In-memory archives.
t = nelson.io.tar.open([], 'w', 'Compression', 'gzip', 'Level', 9);
t.write('x.txt', 'in memory');
bytes = t.tobytes();
asserts.isequal(bytes(1:2), uint8([31 139]));
r = nelson.io.tar.open(bytes);
asserts.isequal(r.Compression, "gzip");
asserts.isequal(r.readtext('x.txt'), 'in memory');
r.close();
asserts.checkerror('nelson.io.tar.open(uint8(1:10))', 'Not a tar archive: <bytes>', 'Nelson:io:tar:notTarArchive');
asserts.checkerror('nelson.io.tar.open([], ''w'', ''Compression'', ''rar'')', ...
  'Compression must be ''auto'', ''none'', ''gzip'', ''zstd'', ''xz'', ''bzip2'', ''lz4'' or ''lzma''.', ...
  'Nelson:io:tar:unknownCompression');
asserts.checkerror('nelson.io.tar.open([], ''w'', ''Compression'', ''xz'', ''Level'', 10)', ...
  'Level must be an integer from 0 to 9.', 'Nelson:io:tar:invalidLevel');
%=============================================================================
% Every compression is written and read back; the file name chooses it with 'auto'.
for c = {'xz', 'bzip2', 'lz4', 'lzma', 'zstd', 'gzip', 'none'}
  t = nelson.io.tar.open([], 'w', 'Compression', c{1});
  t.write('dir/a.txt', repmat('tar text ', 1, 50));
  r = nelson.io.tar.open(t.tobytes());
  asserts.isequal(r.Compression, string(c{1}));
  asserts.isequal(r.readtext('dir/a.txt'), repmat('tar text ', 1, 50));
  r.close();
end
xzfile = [tempname(), '.tar.xz'];
t = nelson.io.tar.open(xzfile, 'w');
asserts.isequal(t.Compression, "xz");
t.write('b.txt', 'xz by name');
t.close();
r = nelson.io.tar.open(xzfile);
asserts.isequal(r.readtext('b.txt'), 'xz by name');
r.close();
delete(xzfile);
%=============================================================================
% Archives written and read by the system tar command.
[status, ~] = system('tar --version');
if status == 0
  % Relative names: some tar commands read 'C:' as a remote host.
  inWork = ['cd "', WORK, '" && '];
  if ispc()
    inWork = ['cd /d "', WORK, '" && '];
  end
  [status, message] = system([inWork, 'tar -czf system.tar.gz src']);
  asserts.isequal(status, 0, message);
  r = nelson.io.tar.open([WORK, '/system.tar.gz']);
  asserts.isequal(r.read('src/sub/b.bin'), uint8(0:255));
  r.close();
  [status, listing] = system([inWork, 'tar -tzf archive.tar.gz']);
  asserts.isequal(status, 0, listing);
  asserts.istrue(contains(listing, 'src/sub/b.bin'));
  asserts.istrue(contains(listing, 'stream.txt'));
  OUT = [WORK, '/system_out'];
  mkdir(OUT);
  [status, message] = system([inWork, 'tar -xzf archive.tar.gz -C system_out']);
  asserts.isequal(status, 0, message);
  asserts.isequal(fileread([OUT, '/src/a.txt']), sprintf('alpha\n'));
end
%=============================================================================
rmdir(WORK, 's');
%=============================================================================
