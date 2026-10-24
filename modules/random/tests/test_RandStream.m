%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s = RandStream('mlfg6331_64', 'Seed', 0);
asserts.isequal(class(s), 'RandStream');
asserts.isequal(s.Type, 'mlfg6331_64');
asserts.isequal(s.Seed, uint32(0));
asserts.isequal(s.Substream, 1);
asserts.checkerror('s.Seed = uint32(2);', _('Cannot set property: Seed'));
%=============================================================================
fractionalSeed = RandStream('mt19937ar', 'Seed', 7.9);
asserts.isequal(fractionalSeed.Seed, uint32(7));
shuffleSeed = RandStream('mt19937ar', 'Seed', 'shuffle');
asserts.isequal(class(shuffleSeed), 'RandStream');
%=============================================================================
props = RandStream('mt19937ar', 'Seed', 2, 'NormalTransform', 'polar', ...
  'Antithetic', true, 'FullPrecision', false);
asserts.isequal(props.NormalTransform, 'Polar');
asserts.istrue(props.Antithetic);
asserts.isfalse(props.FullPrecision);
props.NormalTransform = 'inversion';
props.Antithetic = false;
props.FullPrecision = true;
asserts.isequal(props.NormalTransform, 'Inversion');
asserts.isfalse(props.Antithetic);
asserts.istrue(props.FullPrecision);
%=============================================================================
s64 = RandStream('twister64', 'Seed', 7);
asserts.isequal(s64.Type, 'twister64');
asserts.isequal(size(rand(s64, 1, 2)), [1 2]);
%=============================================================================
types = {'twister', 'twister64', 'simdTwister', 'combRecursive', ...
  'philox', 'threefry', 'laggedfibonacci607', 'mlfg6331_64'};
for k = 1:length(types)
  streamForType = RandStream(types{k}, 'Seed', k);
  asserts.isequal(size(rand(streamForType, 1, 2)), [1 2]);
end
%=============================================================================
a = rand(s, 1, 4);
b = rand(s, 1, 4);
reset(s);
c = rand(s, 1, 4);
asserts.isequal(a, c);
asserts.isfalse(isequal(a, b));
%=============================================================================
s = RandStream('mt19937ar', 'Seed', 3);
rand(s, 1, 2);
savedState = s.State;
expected = rand(s, 1, 2);
s.State = savedState;
asserts.isequal(rand(s, 1, 2), expected);
%=============================================================================
