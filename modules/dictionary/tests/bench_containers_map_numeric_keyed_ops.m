%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
% Bench: containers.Map keyed insert/overwrite/lookup on the native storage
% fast path with NUMERIC keys. Inserts must stay O(1) per key: building a
% numeric-keyed map of a few hundred entries took tens of seconds (N=400 was
% 32 s) before numeric keys reached the native store, and must stay in the
% millisecond range.
%=============================================================================
N = 5000;
%=============================================================================
m = containers.Map('KeyType', 'double', 'ValueType', 'double');
tic();
for i = 1:N
  m(i) = i;
end
t_insert = toc();
asserts.isequal(double(m.Count), N);
fprintf('  keyed insert:    %0.1f ns/op (total %0.1f ms)\n', t_insert / N * 1e9, t_insert * 1000);
%=============================================================================
tic();
for i = 1:N
  m(i) = i + 1;
end
t_overwrite = toc();
asserts.isequal(double(m.Count), N);
fprintf('  keyed overwrite: %0.1f ns/op\n', t_overwrite / N * 1e9);
%=============================================================================
acc = 0;
tic();
for i = 1:N
  acc = acc + m(i);
end
t_lookup = toc();
asserts.isequal(acc, sum(1:N) + N);
fprintf('  keyed lookup:    %0.1f ns/op\n', t_lookup / N * 1e9);
%=============================================================================
% complexity guard: the full loops must stay far below the O(n^2) regime
asserts.istrue(t_insert < 2.0);
asserts.istrue(t_lookup < 2.0);
%=============================================================================
% behavior checks: numeric native path matches the documented semantics
m2 = containers.Map('KeyType', 'int64', 'ValueType', 'any');
m2(int64(1)) = 'one';
m2(int64(2)) = [1 2 3];
m2(int64(1)) = 'uno';
asserts.isequal(m2(int64(1)), 'uno');
asserts.isequal(double(m2.Count), 2);
remove(m2, int64(2));
asserts.isequal(double(m2.Count), 1);
asserts.istrue(~isKey(m2, int64(2)));
ok = false;
try
  x = m2(int64(9));
catch ex
  ok = strcmp(ex.identifier, 'Nelson:Containers:Map:NoKey');
end
asserts.istrue(ok);
%=============================================================================
