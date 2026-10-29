%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
m = containers.Map();
asserts.isequal(class(m), 'containers.Map');
asserts.isequal(m.Count, 0);
asserts.isequal(m.KeyType, 'char');
asserts.isequal(m.ValueType, 'any');
asserts.isequal(keys(m), cell(1, 0));
asserts.isequal(values(m), cell(1, 0));
%=============================================================================
m = containers.Map({'a', 'b'}, [1 2]);
asserts.isequal(class(m), 'containers.Map');
asserts.isequal(m.Count, 2);
asserts.isequal(m.KeyType, 'char');
asserts.isequal(m.ValueType, 'double');
asserts.isequal(m('a'), 1);
asserts.isequal(m('b'), 2);
asserts.isequal(isKey(m, {'a', 'z'}), [true false]);
asserts.isequal(keys(m), {'a', 'b'});
asserts.isequal(values(m), {1, 2});
mNative = containers.Map({'a', 'b'}, [1 2]);
remove(mNative, 'a');
asserts.isequal(mNative.Count, 1);
asserts.isequal(keys(mNative), {'b'});
mNative('c') = 3;
asserts.isequal(mNative.Count, 2);
asserts.isequal(mNative('b'), 2);
asserts.isequal(mNative('c'), 3);
%=============================================================================
mstring = containers.Map([string('sa'), string('sb')], [11 12]);
asserts.isequal(mstring.Count, 2);
asserts.isequal(mstring.KeyType, 'char');
asserts.isequal(mstring.ValueType, 'double');
asserts.isequal(mstring('sa'), 11);
asserts.isequal(mstring('sb'), 12);
%=============================================================================
mncell = containers.Map({1, 2}, {'one', 'two'});
asserts.isequal(mncell.Count, 2);
asserts.isequal(mncell.KeyType, 'double');
asserts.isequal(mncell.ValueType, 'char');
asserts.isequal(mncell(1), 'one');
asserts.isequal(mncell(2), 'two');
%=============================================================================
failed = false;
try
  containers.Map({string('a'), string('b')}, [1 2]);
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
mcol = containers.Map({'c'; 'd'}, [3; 4]);
asserts.isequal(mcol.Count, 2);
asserts.isequal(mcol('c'), 3);
asserts.isequal(mcol('d'), 4);
asserts.isequal(values(mcol), {3, 4});
%=============================================================================
mn = containers.Map([1 2 3], {'a', 'b', 'c'});
asserts.isequal(mn.Count, 3);
asserts.isequal(mn.KeyType, 'double');
asserts.isequal(mn.ValueType, 'char');
asserts.isequal(mn(2), 'b');
asserts.isequal(isKey(mn, {2, 9}), [true false]);
%=============================================================================
mi = containers.Map(int32([1 2]), {10, 20});
asserts.isequal(mi.KeyType, 'int32');
asserts.isequal(mi.ValueType, 'double');
asserts.isequal(mi(int32(1)), 10);
asserts.isequal(mi(1), 10);
%=============================================================================
mu8 = containers.Map(uint8([1 2]), {'one', 'two'});
asserts.isequal(mu8.KeyType, 'double');
asserts.isequal(mu8(1), 'one');
asserts.isequal(mu8(2), 'two');
%=============================================================================
mlogical = containers.Map([false true], {'no', 'yes'});
asserts.isequal(mlogical.KeyType, 'double');
asserts.isequal(mlogical(0), 'no');
asserts.isequal(mlogical(1), 'yes');
%=============================================================================
mtyped = containers.Map('KeyType', 'char', 'ValueType', 'logical');
asserts.isequal(mtyped.Count, 0);
asserts.isequal(keys(mtyped), cell(1, 0));
asserts.isequal(values(mtyped), cell(1, 0));
mtyped('ready') = true;
asserts.isequal(mtyped.ValueType, 'logical');
asserts.istrue(mtyped('ready'));
%=============================================================================
mtypedString = containers.Map("KeyType", "char", "ValueType", "double");
asserts.isequal(mtypedString.Count, 0);
asserts.isequal(mtypedString.KeyType, 'char');
asserts.isequal(mtypedString.ValueType, 'double');
mtypedString('answer') = 42;
asserts.isequal(mtypedString('answer'), 42);
%=============================================================================
supportedKeyTypes = {'char', 'double', 'single', 'int32', 'uint32', 'int64', 'uint64'};
for k = 1:numel(supportedKeyTypes)
  mtypedKey = containers.Map('KeyType', supportedKeyTypes{k}, 'ValueType', 'any');
  asserts.isequal(mtypedKey.Count, 0);
  asserts.isequal(mtypedKey.KeyType, supportedKeyTypes{k});
  asserts.isequal(mtypedKey.ValueType, 'any');
end
supportedValueTypes = {'any', 'char', 'double', 'single', 'int32', 'uint32', 'int64', 'uint64', 'logical', 'int8', 'uint8'};
for k = 1:numel(supportedValueTypes)
  mtypedValue = containers.Map('KeyType', 'char', 'ValueType', supportedValueTypes{k});
  asserts.isequal(mtypedValue.Count, 0);
  asserts.isequal(mtypedValue.KeyType, 'char');
  asserts.isequal(mtypedValue.ValueType, supportedValueTypes{k});
end
%=============================================================================
memptyAssign = containers.Map();
asserts.isequal(keys(memptyAssign), cell(1, 0));
asserts.isequal(values(memptyAssign), cell(1, 0));
memptyAssign('first') = 1;
asserts.isequal(memptyAssign.Count, 1);
asserts.isequal(memptyAssign('first'), 1);
asserts.isequal(keys(memptyAssign), {'first'});
asserts.isequal(values(memptyAssign), {1});
%=============================================================================
failed = false;
try
  containers.Map('KeyType', 'logical', 'ValueType', 'any');
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
failed = false;
try
  containers.Map('KeyType', 'char', 'ValueType', 'bad');
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
failed = false;
try
  isKey(mn, [2 9]);
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
mb = containers.Map({'u', 'v'}, 5);
asserts.isequal(mb.Count, 2);
asserts.isequal(mb('u'), 5);
asserts.isequal(mb('v'), 5);
asserts.isequal(values(mb), {5, 5});
%=============================================================================
m('a') = 9;
asserts.isequal(m('a'), 9);
remove(m, 'a');
asserts.isequal(m.Count, 1);
asserts.isfalse(isKey(m, 'a'));
asserts.istrue(isKey(m, 'b'));
%=============================================================================
m = containers.Map({'x', 'x'}, [1 2]);
asserts.isequal(m.Count, 1);
asserts.isequal(m('x'), 2);
%=============================================================================
m = containers.Map({'a', 'b', 'c'}, 7);
asserts.isequal(m.Count, 3);
asserts.isequal(m('a'), 7);
asserts.isequal(m('b'), 7);
asserts.isequal(values(m), {7, 7, 7});
%=============================================================================
keysToUse = cell(1, 12);
valuesToUse = zeros(1, 12);
for k = 1:12
  keysToUse{k} = ['k', num2str(k)];
  valuesToUse(k) = k;
end
m = containers.Map(keysToUse, valuesToUse);
asserts.isequal(m.Count, 12);
asserts.isequal(m('k12'), 12);
% keys() and values() are returned in sorted-key order (like the reference).
[sortedKeys, sortOrder] = sort(keysToUse);
asserts.isequal(keys(m), sortedKeys);
asserts.isequal(values(m), num2cell(valuesToUse(sortOrder)));
%=============================================================================
m = containers.Map('KeyType', 'char', 'ValueType', 'any');
m('payload') = [1 2 3];
asserts.isequal(m('payload'), [1 2 3]);
%=============================================================================
m = containers.Map('KEYTYPE', 'CHAR', 'VALUETYPE', 'ANY');
asserts.isequal(m.KeyType, 'char');
asserts.isequal(m.ValueType, 'any');
%=============================================================================
m2 = m;
m2('payload') = 42;
asserts.isequal(m('payload'), 42);
%=============================================================================
failed = false;
try
  m.Count = 3;
catch ex
  failed = true;
  asserts.isequal(ex.message, _('Property ''Count'' is read-only.'));
end
asserts.istrue(failed);
%=============================================================================
failed = false;
try
  m('missing');
catch ex
  failed = true;
  asserts.isequal(ex.message, _('The specified key is not present in this container.'));
end
asserts.istrue(failed);
%=============================================================================
% keys()/values() are returned in sorted-key order, not insertion order.
mSortChar = containers.Map({'banana', 'apple', 'cherry'}, {2, 1, 3});
asserts.isequal(keys(mSortChar), {'apple', 'banana', 'cherry'});
asserts.isequal(values(mSortChar), {1, 2, 3});
mSortNum = containers.Map('KeyType', 'double', 'ValueType', 'double');
mSortNum(5) = 50; mSortNum(1) = 10; mSortNum(3) = 30;
asserts.isequal(cell2mat(keys(mSortNum)), [1 3 5]);
asserts.isequal(cell2mat(values(mSortNum)), [10 30 50]);
%=============================================================================
% length returns the number of entries and isempty/size follow Count
mLen = containers.Map({'a', 'b', 'c'}, {1, 2, 3});
asserts.isequal(length(mLen), 3);
asserts.isequal(numel(mLen), 1);
asserts.isequal(size(mLen), [3 1]);
asserts.isfalse(isempty(mLen));
%=============================================================================
mEmptyLen = containers.Map();
asserts.isequal(length(mEmptyLen), 0);
asserts.isequal(size(mEmptyLen), [0 1]);
asserts.istrue(isempty(mEmptyLen));
%=============================================================================
% values with a key subset returns the values for those keys, in order
mVal = containers.Map({'a', 'b', 'c'}, {10, 20, 30});
asserts.isequal(cell2mat(values(mVal, {'c', 'a'})), [30 10]);
asserts.isequal(cell2mat(values(mVal)), [10 20 30]);
%=============================================================================
% isequal and isequaln compare map contents, not handle identity
m1 = containers.Map({'a', 'b'}, {1, 2});
m2 = containers.Map({'b', 'a'}, {2, 1});
asserts.istrue(isequal(m1, m2));
asserts.isfalse(isequal(m1, containers.Map({'a', 'b'}, {1, 3})));
asserts.isfalse(isequal(m1, containers.Map({'a'}, {1})));
asserts.isfalse(isequal(m1, 5));
asserts.istrue(isequal(containers.Map(), containers.Map()));
asserts.istrue(isequal(m1, m2, containers.Map({'a', 'b'}, {1, 2})));
%=============================================================================
% NaN values differ under isequal but match under isequaln
mn1 = containers.Map({'a'}, {NaN});
mn2 = containers.Map({'a'}, {NaN});
asserts.isfalse(isequal(mn1, mn2));
asserts.istrue(isequaln(mn1, mn2));
%=============================================================================
% UniformValues = false builds a map that does not enforce a value type.
muv = containers.Map('UniformValues', false);
asserts.isequal(muv.Count, 0);
asserts.isequal(muv.KeyType, 'char');
asserts.isequal(muv.ValueType, 'any');
asserts.isequal(keys(muv), cell(1, 0));
asserts.isequal(values(muv), cell(1, 0));
muv('a') = {1, 2};
asserts.isequal(numel(muv('a')), 2);
muv('b') = [10 20 30];
asserts.isequal(muv('b'), [10 20 30]);
muv('c') = 'text';
asserts.isequal(muv('c'), 'text');
muv('d') = int8(5);
asserts.isequal(class(muv('d')), 'int8');
% Value type stays 'any' regardless of the values that were stored.
asserts.isequal(muv.ValueType, 'any');
asserts.isequal(muv.Count, 4);
asserts.isequal(keys(muv), {'a', 'b', 'c', 'd'});
asserts.istrue(isKey(muv, 'b'));
asserts.isfalse(isKey(muv, 'z'));
% values() still returns a cell array.
allValues = values(muv);
asserts.istrue(iscell(allValues));
asserts.isequal(numel(allValues), 4);
%=============================================================================
% UniformValues = false accepts numeric arrays of different sizes per key.
mSizes = containers.Map('UniformValues', false);
mSizes('one') = 1;
mSizes('row') = [1 2 3];
mSizes('mat') = [1 2; 3 4];
asserts.isequal(size(mSizes('one')), [1 1]);
asserts.isequal(size(mSizes('row')), [1 3]);
asserts.isequal(size(mSizes('mat')), [2 2]);
%=============================================================================
% The 'UniformValues' name is accepted as a string scalar and case-insensitively.
muvString = containers.Map("UniformValues", false);
muvString('x') = {1, 2, 3};
asserts.isequal(numel(muvString('x')), 3);
muvCase = containers.Map('uniformvalues', false);
asserts.isequal(muvCase.ValueType, 'any');
% A numeric option value is interpreted as a logical.
muvNumeric = containers.Map('UniformValues', 0);
muvNumeric('a') = {1, 2};
asserts.isequal(numel(muvNumeric('a')), 2);
%=============================================================================
% UniformValues = false together with keys and values forces the value type
% to 'any', even when the supplied values share a common type.
muvSame = containers.Map({'a', 'b'}, {1, 2}, 'UniformValues', false);
asserts.isequal(muvSame.ValueType, 'any');
asserts.isequal(muvSame.Count, 2);
muvHetero = containers.Map({'a', 'b'}, {1, [2 3]}, 'UniformValues', false);
asserts.isequal(muvHetero.ValueType, 'any');
asserts.isequal(numel(muvHetero('b')), 2);
%=============================================================================
% UniformValues = true keeps the value type enforcement in place.
muvTrue = containers.Map({'a'}, {1}, 'UniformValues', true);
asserts.isequal(muvTrue.ValueType, 'double');
failed = false;
try
  muvTrue('b') = {1, 2};
catch
  failed = true;
end
asserts.istrue(failed);
% An empty map built with UniformValues = true holds no entries.
muvTrueEmpty = containers.Map('UniformValues', true);
asserts.isequal(muvTrueEmpty.Count, 0);
%=============================================================================
% 'UniformValues' cannot be combined with explicit KeyType/ValueType options.
failed = false;
try
  containers.Map('KeyType', 'char', 'ValueType', 'double', 'UniformValues', false);
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
