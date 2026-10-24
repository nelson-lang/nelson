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
% fast path. Inserts must stay O(1) per key: the whole 5000-key insert loop
% ran in minutes before native '__set' existed, and must stay in the
% millisecond range.
%=============================================================================
N = 5000;
ks = cell(1, N);
for i = 1:N
  ks{i} = sprintf('key%d', i);
end
%=============================================================================
m = containers.Map('KeyType', 'char', 'ValueType', 'double');
tic();
for i = 1:N
  key = ks{i};
  m(key) = i;
end
t_insert = toc();
asserts.isequal(m.Count, N);
fprintf('  keyed insert:    %0.1f ns/op (total %0.1f ms)\n', t_insert / N * 1e9, t_insert * 1000);
%=============================================================================
tic();
for i = 1:N
  key = ks{i};
  m(key) = i + 1;
end
t_overwrite = toc();
asserts.isequal(m.Count, N);
fprintf('  keyed overwrite: %0.1f ns/op\n', t_overwrite / N * 1e9);
%=============================================================================
acc = 0;
tic();
for i = 1:N
  key = ks{i};
  acc = acc + m(key);
end
t_lookup = toc();
asserts.isequal(acc, sum(1:N) + N);
fprintf('  keyed lookup:    %0.1f ns/op\n', t_lookup / N * 1e9);
%=============================================================================
% complexity guard: the full loops must stay far below the O(n^2) regime
% (165 s insert / 2.6 s lookup before the native storage rework)
asserts.istrue(t_insert < 1.0);
asserts.istrue(t_lookup < 1.0);
%=============================================================================
% behavior checks: fast path and .m path must agree
m2 = containers.Map();
m2('alpha') = 1;
m2('beta') = [1 2 3];
m2('alpha') = 'text';
asserts.isequal(m2('alpha'), 'text');
asserts.isequal(m2.Count, 2);
remove(m2, 'beta');
asserts.isequal(m2.Count, 1);
asserts.istrue(~isKey(m2, 'beta'));
ok = false;
try
  x = m2('missing');
catch ex
  ok = strcmp(ex.identifier, 'Nelson:Containers:Map:NoKey');
end
asserts.istrue(ok);
%=============================================================================
