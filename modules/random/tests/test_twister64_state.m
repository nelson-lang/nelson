%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% twister64 behaves like twister: the saved state alone fixes the sequence.
%=============================================================================
% state layout: the 312 most recent 64-bit words, oldest first
word = @(hi, lo) uint64(hi) * uint64(4294967296) + uint64(lo);
rng(7, 'twister64');
st = rng();
asserts.isequal(size(st.State), [312 1]);
asserts.isequal(class(st.State), 'uint64');
asserts.isequal(st.State([1 2 312])', [uint64(7), word(1782426941, 404126268), word(782214229, 1959931450)]);
asserts.isapprox(rand(1, 3), [0.75438530415285798 0.94930120289264419 0.11741428103451801], 2e-16);
%=============================================================================
% state saved in the middle of a 312-word block
rand(1, 400);
st = rng();
asserts.isequal(st.State([1 2 312])', [word(1850266453, 178321286), ...
    word(3868579187, 3323643826), word(705129874, 1938095262)]);
x = rand(1, 500);
rng(st);
asserts.isequal(rand(1, 500), x);
%=============================================================================
rng(0, 'twister64');
st = rng();
asserts.isequal(st.Seed, uint64(0));
asserts.isequal(st.State([1 312])', [uint64(5489), word(3327846748, 1168496534)]);
%=============================================================================
% randn keeps no value outside the state
rng(1, 'twister64');
randn(1, 1);
st = rng();
x = randn(1, 5);
rng(st);
asserts.isequal(randn(1, 5), x);
rng(1, 'twister64');
x = [randn(1, 1), randn(1, 1), randn(1, 1)];
rng(1, 'twister64');
asserts.isequal(randn(1, 3), x);
s = RandStream('twister64', 'Seed', 1);
x = [randn(s, 1, 1), randn(s, 1, 1)];
reset(s);
asserts.isequal(randn(s, 1, 2), x);
%=============================================================================
% standard normal moments
rng(3, 'twister64');
x = randn(1, 1e6);
asserts.istrue(abs(mean(x)) < 5e-3);
asserts.isapprox(std(x), 1, 5e-3);
asserts.isequal(class(randn(2, 'single')), 'single');
%=============================================================================
