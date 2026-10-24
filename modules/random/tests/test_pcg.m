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
rng('pcg');
s = rng();
assert_isequal(s.Type, 'pcg');
assert_isequal(s.Seed, uint32(0));
assert_isequal(class(s.State), 'uint64');
assert_isequal(size(s.State), [6 1]);
% 128-bit state and increment expanded from seed 0, full precision, empty cache.
HI = uint64([4169657933 1255950669 227064369 4044845392]');
LO = uint64([321218009 456477130 1215135 3835233241]');
REF = [bitshift(HI, 32) + LO; uint64(0); uint64(0)];
assert_isequal(s.State, REF);
%=============================================================================
rng(0, 'pcg64dxsm');
s = rng();
assert_isequal(s.Type, 'pcg');
rng('default');
rng('pcg64dxsm');
s = rng();
assert_isequal(s.Type, 'pcg');
lst = rng('enginelist');
assert_istrue(any(strcmp(lst, 'pcg')));
%=============================================================================
% 53 random bits of each 64-bit DXSM output.
rng(0, 'pcg');
R = rand(1, 4);
REF = [0.62777934108007283 0.64937882380237077 0.58626073552064784 0.63619222094334527];
assert_isequal(R, REF);
rng(42, 'pcg');
R = rand(2, 2);
REF = [0.25111547288804958 0.25357711869610622; 0.82460521729518999 0.17092880280959055];
assert_isequal(R, REF);
%=============================================================================
% reproducibility and state save / restore
rng(7, 'pcg');
A = rand(3, 5);
rng(7, 'pcg');
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
rng(3, 'pcg');
U = rand(1, 10000);
assert_istrue(all(U > 0 & U < 1));
assert_istrue(abs(mean(U) - 0.5) < 0.02);
rng(3, 'pcg');
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
s.State(4) = uint64(2);
assert_checkerror('rng(s)', _('Must contain generator settings captured previously.'), 'Nelson:rng:invalidSettings');
%=============================================================================
rng(backup);
%=============================================================================
