%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
backup = rng();
%=============================================================================
rng('xoshiro');
s = rng();
assert_isequal(s.Type, 'xoshiro');
assert_isequal(s.Seed, uint32(0));
assert_isequal(class(s.State), 'uint64');
assert_isequal(size(s.State), [6 1]);
% 256-bit state expanded from seed 0 by SplitMix64, full precision, empty cache.
HI = uint64([3793791033 1853398634 113532184 4169906344]');
LO = uint64([2065550767 2713282036 2148091215 1917616620]');
REF = [bitshift(HI, 32) + LO; uint64(0); uint64(0)];
assert_isequal(s.State, REF);
%=============================================================================
rng(0, 'xoshiro256pp');
s = rng();
assert_isequal(s.Type, 'xoshiro');
rng('default');
rng('xoshiro256pp');
s = rng();
assert_isequal(s.Type, 'xoshiro');
lst = rng('enginelist');
assert_istrue(any(strcmp(lst, 'xoshiro')));
%=============================================================================
% 53 random bits of each 64-bit xoshiro256++ output.
rng(0, 'xoshiro');
R = rand(1, 4);
REF = [0.32457526803140668 0.38223929651167343 0.35961720764735527 0.011455508934653635];
assert_isequal(R, REF);
rng(42, 'xoshiro');
R = rand(2, 2);
REF = [0.81430514512290986 0.98389416817748876; 0.31882104006166112 0.70113559813475557];
assert_isequal(R, REF);
%=============================================================================
% reproducibility and state save / restore
rng(7, 'xoshiro');
A = rand(3, 5);
rng(7, 'xoshiro');
B = rand(3, 5);
assert_isequal(A, B);
s = rng();
A = [rand(1, 5), randn(1, 5), randi(10, 1, 5)];
rng(s);
B = [rand(1, 5), randn(1, 5), randi(10, 1, 5)];
assert_isequal(A, B);
C = rand(1, 5);
assert_isfalse(isequal(C, A(1:5)));
%=============================================================================
% uniform values in (0, 1), normal values by inversion of the uniforms
rng(3, 'xoshiro');
U = rand(1, 10000);
assert_istrue(all(U > 0 & U < 1));
assert_istrue(abs(mean(U) - 0.5) < 0.02);
rng(3, 'xoshiro');
Z = randn(1, 10000);
assert_isapprox(Z, norminv(U), 1e-12);
assert_istrue(abs(mean(Z)) < 0.05);
assert_istrue(abs(std(Z) - 1) < 0.05);
Z = randn(2, 3, 'like', 1i);
assert_isfalse(isreal(Z));
assert_isequal(size(Z), [2 3]);
%=============================================================================
X = rand(100, 10, 'single');
assert_isequal(class(X), 'single');
assert_istrue(all(X(:) > 0 & X(:) < 1));
X = randn(3, 'single');
assert_isequal(class(X), 'single');
%=============================================================================
R = randi([-3 4], 1, 2000);
assert_istrue(all(R >= -3 & R <= 4));
assert_isequal(unique(R), -3:4);
P = randperm(20);
assert_isequal(sort(P), 1:20);
P = randperm(100, 3);
assert_isequal(numel(unique(P)), 3);
%=============================================================================
% invalid states
s = rng();
s.State = uint32(s.State);
assert_checkerror('rng(s)', _('Type of state must be ''uint64''.'));
s = rng();
s.State(1:4) = uint64(0);
assert_checkerror('rng(s)', _('Must contain generator settings captured previously.'), 'Nelson:rng:invalidSettings');
%=============================================================================
rng(backup);
%=============================================================================
