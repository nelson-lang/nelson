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
txt = evalc('RandStream.list');
assert_istrue(contains(txt, 'pcg64dxsm'));
assert_istrue(contains(txt, 'xoshiro256pp'));
%=============================================================================
types = {'pcg64dxsm', 'pcg', 'xoshiro256pp', 'xoshiro'};
names = {'pcg64dxsm', 'pcg64dxsm', 'xoshiro256pp', 'xoshiro256pp'};
internals = {'pcg', 'pcg', 'xoshiro', 'xoshiro'};
for k = 1:numel(types)
  rng(42, internals{k});
  REF = rand(1, 4);
  s = RandStream(types{k}, 'Seed', 42);
  assert_isequal(s.Type, names{k});
  assert_isequal(s.NormalTransform, 'Inversion');
  assert_istrue(s.FullPrecision);
  assert_isequal(rand(s, 1, 4), REF);
  % randn: inverse normal CDF of the uniform draws
  s = RandStream(types{k}, 'Seed', 5);
  z = randn(s, 1, 100);
  reset(s);
  assert_isapprox(z, norminv(rand(s, 1, 100)), 1e-12);
end
%=============================================================================
s = RandStream('pcg', 'Seed', 42);
assert_isequal(rand(s, 1, 2), [0.25111547288804958 0.82460521729518999]);
s = RandStream('xoshiro', 'Seed', 42);
assert_isequal(rand(s, 1, 2), [0.81430514512290986 0.31882104006166112]);
%=============================================================================
% FullPrecision false: each double from a 32-bit half output (two per output).
s = RandStream('pcg64dxsm', 'Seed', 42, 'FullPrecision', false);
assert_isfalse(s.FullPrecision);
REF = [0.25111547287087888 0.57374773256015033 0.82460521731991321 0.39381489774677902];
assert_isequal(rand(s, 1, 4), REF);
s = RandStream('xoshiro256pp', 'Seed', 42, 'FullPrecision', false);
REF = [0.81430514517705888 0.2674317731289193 0.31882104009855539 0.3415407877182588];
assert_isequal(rand(s, 1, 4), REF);
% the cached half output is kept between calls
reset(s);
A = [rand(s, 1, 3), rand(s, 1, 3)];
reset(s);
B = rand(s, 1, 6);
assert_isequal(A, B);
assert_isequal(A(1:4), REF);
x = A * 2 ^ 33;
assert_isequal(mod(x, 2), ones(1, 6));
% toggling the property
s.FullPrecision = true;
reset(s);
assert_isequal(rand(s, 1, 2), [0.81430514512290986 0.31882104006166112]);
s.FullPrecision = false;
reset(s);
assert_isequal(rand(s, 1, 4), REF);
%=============================================================================
% substreams and multiple streams
s = RandStream('pcg', 'Seed', 1);
a = rand(s, 1, 5);
s.Substream = 2;
b = rand(s, 1, 5);
assert_isfalse(isequal(a, b));
s.Substream = 1;
assert_isequal(rand(s, 1, 5), a);
[s1, s2] = RandStream.create('xoshiro256pp', 'NumStreams', 2, 'Seed', 3);
assert_isequal(s1.Type, 'xoshiro256pp');
assert_isequal(s2.StreamIndex, 2);
assert_isfalse(isequal(rand(s1, 1, 5), rand(s2, 1, 5)));
%=============================================================================
% global stream and state roundtrip
s = RandStream('xoshiro', 'Seed', 9);
previous = RandStream.setGlobalStream(s);
st = s.State;
assert_isequal(class(st), 'uint64');
a = rand(1, 4);
s.State = st;
assert_isequal(rand(1, 4), a);
g = RandStream.getGlobalStream();
assert_isequal(g.Type, 'xoshiro256pp');
RandStream.setGlobalStream(previous);
%=============================================================================
p = randperm(RandStream('pcg', 'Seed', 2), 10);
assert_isequal(sort(p), 1:10);
r = randi(RandStream('xoshiro', 'Seed', 2), 6, 1, 100);
assert_istrue(all(r >= 1 & r <= 6));
%=============================================================================
rng(backup);
%=============================================================================
