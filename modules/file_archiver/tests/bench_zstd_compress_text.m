%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
data = uint8(sprintf('%d,%0.6f;', [1:1e6; (1:1e6) / 7]));
tic();
bytes = nelson.io.zstd.compress(data, 'Level', 6);
elapsed = toc();
ops = numel(data);
asserts.isequal(nelson.io.zstd.decompress(bytes), data);
fprintf('BENCH_REFERENCE,file_archiver,zstd compress 19 MB text,%.9g,%d,%.9g,OK\n', elapsed, int32(ops), ops / elapsed);
%=============================================================================
