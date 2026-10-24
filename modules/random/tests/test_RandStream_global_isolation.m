%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Creating or drawing from a RandStream that is not the global stream must
% leave the global generator untouched.
%=============================================================================
types = {'mt19937ar', 'twister64', 'dsfmt19937', 'mrg32k3a', 'mlfg6331_64', ...
  'philox4x32_10', 'threefry4x64_20', 'pcg64dxsm', 'xoshiro256pp'};
internalTypes = {'twister', 'twister64', 'simdTwister', 'combRecursive', ...
  'laggedfibonacci607', 'philox', 'threefry', 'pcg', 'xoshiro'};
for k = 1:length(types)
  rng(42, internalTypes{k});
  expected = rand(1, 4);
  rng(42, internalTypes{k});
  before = rng();
  s = RandStream(types{k}, 'Seed', 42);
  asserts.isequal(rng(), before);
  asserts.isequal(rand(s, 1, 4), expected);
  asserts.isequal(rng(), before);
  randn(s, 2, 3);
  randi(s, 10, 1, 5);
  randperm(s, 6);
  asserts.isequal(rng(), before);
end
%=============================================================================
% the private stream keeps its own sequence across calls
rng(3, 'twister');
expected = rand(1, 6);
rng('default');
before = rng();
s = RandStream('mt19937ar', 'Seed', 3);
asserts.isequal([rand(s, 1, 2), rand(s, 1, 4)], expected);
asserts.isequal(rng(), before);
%=============================================================================
% options that go through the generation path
before = rng();
s = RandStream('pcg64dxsm', 'Seed', 5, 'FullPrecision', false);
rand(s, 1, 3);
s = RandStream('mrg32k3a', 'Seed', 5, 'Substream', 3);
rand(s, 1, 3);
s = RandStream('mt19937ar', 'Seed', 5, 'NormalTransform', 'Inversion', 'Antithetic', true);
randn(s, 1, 3);
rand(s, 1, 3);
reset(s, 8);
rand(s, 1, 3);
asserts.isequal(rng(), before);
%=============================================================================
% an error raised while drawing leaves the global generator untouched
before = rng();
s = RandStream('philox4x32_10', 'Seed', 1);
asserts.checkerror('rand(s, ''int8'')', _('''single'' or ''double'' expected at last argument.'));
asserts.isequal(rng(), before);
asserts.isequal(rand(s, 1, 2), RandStream('philox4x32_10', 'Seed', 1).rand(1, 2));
%=============================================================================
% the global stream draws from the global generator
rng(11, 'twister');
g = RandStream.getGlobalStream();
first = rand(1, 2);
second = rand(g, 1, 2);
rng(11, 'twister');
asserts.isequal([first, second], rand(1, 4));
%=============================================================================
