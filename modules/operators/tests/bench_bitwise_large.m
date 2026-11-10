%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
N = 1e7;
X = floor(rand(1, N) * 1e6);
Y = floor(rand(1, N) * 1e6);
U8 = uint8(mod(X, 256));
I32 = int32(X);
U64 = uint64(X);
tic(); R = bitand(X, Y); toc();
tic(); R = bitxor(X, 7); toc();
tic(); R = bitor(X, Y, 'int32'); toc();
tic(); R = bitand(U8, U8); toc();
tic(); R = bitxor(I32, I32); toc();
tic(); R = bitor(U64, U64); toc();
tic(); R = bitshift(X, 2); toc();
tic(); R = bitshift(I32, -3); toc();
tic(); R = bitcmp(X, 'uint32'); toc();
tic(); R = bitcmp(U8); toc();
tic(); R = bitget(X, 3); toc();
tic(); R = bitset(X, 4); toc();
tic(); R = bitset(U8, 2, 0); toc();
tic(); R = bitset(X, 3, mod(X, 2)); toc();
R1 = floor(rand(1, 4000) * 1e6);
C1 = floor(rand(2500, 1) * 1e6);
tic(); R = bitand(R1, C1); toc();
tic(); R = bitxor(uint32(R1), uint32(C1)); toc();
%=============================================================================
