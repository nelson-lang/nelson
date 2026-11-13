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
mkdir([WORK, '/data/inner']);
fid = fopen([WORK, '/data/one.txt'], 'w');
fprintf(fid, 'one');
fclose(fid);
fid = fopen([WORK, '/data/inner/two.txt'], 'w');
fprintf(fid, 'two');
fclose(fid);
%=============================================================================
% tar and untar, plain and compressed after the extension.
for ext = {'.tar', '.tgz', '.tar.gz'}
  TARFILE = [WORK, '/pack', ext{1}];
  names = tar(TARFILE, 'data', WORK);
  asserts.isequal(sort(names), sort({'data/inner/two.txt', 'data/one.txt'}));
  OUT = [WORK, '/out', strrep(ext{1}, '.', '_')];
  files = untar(TARFILE, OUT);
  asserts.isequal(numel(files), 2);
  asserts.isequal(fileread([OUT, '/data/inner/two.txt']), 'two');
end
tar([WORK, '/noext'], 'data', WORK);
asserts.istrue(isfile([WORK, '/noext.tar']));
asserts.checkerror('tar([WORK, ''/x.tar''], ''missing_file.txt'', WORK)', 'Invalid value.', 'Nelson:zip:invalidValue');
asserts.checkerror('untar([WORK, ''/data/one.txt''])', ['Not a tar archive: ', WORK, '/data/one.txt'], 'Nelson:untar:notTarArchive');
%=============================================================================
% gzip and gunzip.
gz = gzip([WORK, '/data'], [WORK, '/gzout']);
asserts.isequal(numel(gz), 2);
asserts.istrue(isfile([WORK, '/gzout/two.txt.gz']));
gz = gzip({[WORK, '/data/one.txt'], [WORK, '/data/inner/two.txt']});
asserts.isequal(numel(gz), 2);
asserts.istrue(isfile([WORK, '/data/one.txt.gz']));
out = gunzip([WORK, '/gzout/*.gz'], [WORK, '/plain']);
asserts.isequal(numel(out), 2);
asserts.isequal(fileread([WORK, '/plain/one.txt']), 'one');
asserts.checkerror('gzip([WORK, ''/nothing*.txt''])', ['File not found: ', WORK, '/nothing*.txt'], 'Nelson:gzip:fileNotFound');
%=============================================================================
rmdir(WORK, 's');
%=============================================================================
