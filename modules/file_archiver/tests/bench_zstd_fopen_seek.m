%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
data = uint8(mod((1:8e6) * 7, 251));
fid = nelson.io.zstd.fopen(nelson.io.zstd.compress(data));
tic();
for k = 1:200
  p = mod(k * 7919 * 97, 7e6);
  fseek(fid, p, 'bof');
  value = fread(fid, 4, 'uint8=>uint8')';
end
elapsed = toc();
fclose(fid);
ops = 200;
asserts.isequal(value, data(p + 1:p + 4));
fprintf('BENCH_REFERENCE,file_archiver,zstd fid random seek,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
