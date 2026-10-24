%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Seeded 'twister' randn must reproduce the reference standard-normal stream
% bit-for-bit (256-layer ziggurat over the Mersenne-Twister uint32 stream).
%=============================================================================
rng(42, 'twister');
R = randn(1, 5);
REF = [-5.382438937292696e-01, 8.672321576295733e-01, 9.759864634725395e-01, ...
    3.373902523792121e-01, -9.960940966149540e-01];
asserts.isapprox(R, REF, 1e-13);
%=============================================================================
rng(1, 'twister');
R = randn(1, 5);
REF = [-6.490137651912407e-01, 1.181166041965532e+00, -7.584532972836916e-01, ...
    -1.109613038501522e+00, -8.455512400077967e-01];
asserts.isapprox(R, REF, 1e-13);
%=============================================================================
rng(123, 'twister');
R = randn(1, 6);
REF = [7.643109376924350e-01, -6.049952538315488e-01, -1.034991807064387e+00, ...
    2.014097366275818e-01, 6.679770866856835e-01, -3.234871279175406e-01];
asserts.isapprox(R, REF, 1e-13);
%=============================================================================
% The base uniform stream must stay untouched by the normal generator change.
rng(42, 'twister');
U = rand(1, 3);
REF = [3.745401188473625e-01, 9.507143064099162e-01, 7.319939418114051e-01];
asserts.isapprox(U, REF, 1e-13);
%=============================================================================
% Re-seeding reproduces the exact same normal sequence.
rng(2024, 'twister');
A = randn(1, 50);
rng(2024, 'twister');
B = randn(1, 50);
asserts.isequal(A, B);
%=============================================================================
% Matrix fill is column-major and consistent with element-wise generation.
rng(7, 'twister');
M = randn(2, 2);
rng(7, 'twister');
v = [randn(); randn(); randn(); randn()];
asserts.isequal(M(:), v);
%=============================================================================
