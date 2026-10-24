%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Numeric-keyed containers.Map keep every semantics of the interpreted
% fallback while building, looking up and removing keys in O(1) through the
% native storage (see bench_containers_map_numeric_keyed_ops).
%=============================================================================
% build at scale: keys come back sorted and with the original type
N = 2000;
m = containers.Map('KeyType', 'double', 'ValueType', 'double');
for k = N:-1:1
  m(k) = k * 10;
end
asserts.isequal(double(m.Count), N);
k = keys(m);
asserts.istrue(iscell(k));
asserts.isequal(class(k{1}), 'double');
asserts.isequal(k{1}, 1);
asserts.isequal(k{end}, N);
asserts.istrue(issorted(cell2mat(k)));
v = values(m);
asserts.isequal(v{1}, 10);
asserts.isequal(v{end}, N * 10);
asserts.isequal(m(1), 10);
asserts.isequal(m(N), N * 10);
asserts.istrue(isKey(m, 1));
asserts.istrue(isKey(m, N));
asserts.isfalse(isKey(m, N + 1));
%=============================================================================
% overwrite keeps the key count and takes the last value
m(1) = 999;
asserts.isequal(double(m.Count), N);
asserts.isequal(m(1), 999);
%=============================================================================
% remove a batch, Count and keys follow
remove(m, {1, 2, 3});
asserts.isequal(double(m.Count), N - 3);
asserts.isfalse(isKey(m, 2));
asserts.istrue(isKey(m, 4));
%=============================================================================
% -0 and 0 are the same key
m = containers.Map('KeyType', 'double', 'ValueType', 'double');
m(0) = 1;
m(-0.0) = 2;
asserts.isequal(double(m.Count), 1);
asserts.isequal(m(0), 2);
%=============================================================================
% every supported numeric key type round-trips its key type and value
types = {'double', 'single', 'int32', 'uint32', 'int64', 'uint64'};
for t = 1:numel(types)
  tn = types{t};
  m = containers.Map('KeyType', tn, 'ValueType', 'double');
  key1 = cast(3, tn);
  key2 = cast(1, tn);
  key3 = cast(2, tn);
  m(key1) = 30;
  m(key2) = 10;
  m(key3) = 20;
  asserts.isequal(double(m.Count), 3);
  kk = keys(m);
  asserts.isequal(class(kk{1}), tn);
  asserts.isequal(kk{1}, cast(1, tn));
  asserts.isequal(kk{2}, cast(2, tn));
  asserts.isequal(kk{3}, cast(3, tn));
  asserts.isequal(values(m), {10, 20, 30});
  asserts.isequal(m(cast(2, tn)), 20);
  asserts.istrue(isKey(m, cast(2, tn)));
end
%=============================================================================
% int64 / uint64 keys that do not fit exactly in a double stay distinct
m = containers.Map('KeyType', 'int64', 'ValueType', 'double');
a = int64(9223372036854775807);
b = int64(9223372036854775806);
m(a) = 1;
m(b) = 2;
asserts.isequal(double(m.Count), 2);
asserts.isequal(m(a), 1);
asserts.isequal(m(b), 2);
%=============================================================================
% construction from (keys, values) with numeric keys
m = containers.Map({10, 30, 20}, {'a', 'c', 'b'});
asserts.isequal(m.KeyType, 'double');
asserts.isequal(double(m.Count), 3);
asserts.isequal(keys(m), {10, 20, 30});
asserts.isequal(values(m), {'a', 'b', 'c'});
%=============================================================================
% construction with a non-standard value type keeps working
m = containers.Map({1, 2}, {int16(3), int16(4)});
asserts.isequal(m.ValueType, 'int16');
asserts.isequal(m(2), int16(4));
%=============================================================================
% NaN keys remain rejected, missing lookups and wrong key types still error
m = containers.Map('KeyType', 'double', 'ValueType', 'double');
gotId = '';
try
  m(NaN) = 1;
catch ex
  gotId = ex.identifier;
end
asserts.isequal(gotId, 'Nelson:containersMap:nanKey');
m(1) = 1;
gotId = '';
try
  x = m(42);
catch ex
  gotId = ex.identifier;
end
asserts.isequal(gotId, 'Nelson:Containers:Map:NoKey');
gotId = '';
try
  m('a') = 1;
catch ex
  gotId = ex.identifier;
end
asserts.isequal(gotId, 'Nelson:containersMap:keyTypeMismatch');
%=============================================================================
