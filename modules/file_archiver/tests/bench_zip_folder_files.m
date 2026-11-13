%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = [tempname(), '_bench_zip'];
mkdir([d, '/src']);
for k = 1:64
  fid = fopen(sprintf('%s/src/f%02d.txt', d, k), 'w');
  fprintf(fid, '%d,%0.6f;\n', [1:10000; (1:10000) * k / 7]);
  fclose(fid);
end
tic();
zip([d, '/a.zip'], 'src', d);
elapsed = toc();
ops = 64;
z = nelson.io.zip.open([d, '/a.zip']);
asserts.isequal(height(z.entries()), 64);
z.close();
rmdir(d, 's');
fprintf('BENCH_REFERENCE,file_archiver,zip 64 text files,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
