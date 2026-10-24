%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%=============================================================================
% Ties cross the short-array sorting boundary; expected indices are analytic.
n = 60;
a = mod((1:n)', 3);
ascending = [3:3:n, 1:3:n, 2:3:n]';
descending = [2:3:n, 1:3:n, 3:3:n]';
classes = {'double', 'single', 'int8', 'int16', 'int32', 'int64', ...
  'uint8', 'uint16', 'uint32', 'uint64', 'char'};
for k = 1:numel(classes)
  value = feval(classes{k}, a);
  alias = value;
  [b, i] = sort(value);
  asserts.isequal(i, ascending);
  asserts.isequal(b, value(ascending));
  asserts.isequal(value, feval(classes{k}, mod((1:n)', 3)));
  asserts.isequal(alias, feval(classes{k}, mod((1:n)', 3)));
  [b, i] = sort(value, 'descend');
  asserts.isequal(i, descending);
  asserts.isequal(b, value(descending));
  [b, i] = sort(value');
  asserts.isequal(i, ascending');
  asserts.isequal(b, value(ascending)');
end
value = logical(a);
[b, i] = sort(value);
asserts.isequal(i, [find(~value); find(value)]);
[b, i] = sort(value, 'descend');
asserts.isequal(i, [find(value); find(~value)]);
% Matrix and N-D index arrays describe positions along the requested dimension.
value = reshape([a; a + 10; a + 20; a + 30], n, 2, 2);
[b, i] = sort(value, 1, 'descend');
asserts.isequal(i, repmat(descending, 1, 2, 2));
asserts.isequal(b, value(descending, :, :));
value = [a'; a' + 10];
[b, i] = sort(value, 2);
asserts.isequal(i, repmat(ascending', 2, 1));
asserts.isequal(b, value(:, ascending));
% Missing values and signed zeros retain their input order in every placement.
a = [NaN, 2, -0, NaN, 2, 0, -Inf, Inf, -0, NaN];
finiteAscending = [7, 3, 6, 9, 2, 5, 8];
finiteDescending = [8, 2, 5, 3, 6, 9, 7];
missingIndices = [1, 4, 10];
for type = {'double', 'single'}
  value = feval(type{1}, a);
  for direction = {'ascend', 'descend'}
    for placement = {'auto', 'first', 'last'}
      finiteIndices = finiteAscending;
      if strcmp(direction{1}, 'descend'), finiteIndices = finiteDescending; end
      missingFirst = strcmp(placement{1}, 'first') || ...
        (strcmp(placement{1}, 'auto') && strcmp(direction{1}, 'descend'));
      expected = [finiteIndices, missingIndices];
      if missingFirst, expected = [missingIndices, finiteIndices]; end
      [b, i] = sort(value, direction{1}, 'MissingPlacement', placement{1});
      asserts.isequal(i, expected);
      asserts.isequal(b, value(expected));
    end
  end
end
% Empty, scalar and existing rejected options keep their contract.
[b, i] = sort(zeros(0, 3));
asserts.isequal(size(b), [0, 3]);
asserts.isequal(size(i), [0, 3]);
[b, i] = sort(uint64(9));
asserts.isequal(b, uint64(9));
asserts.isequal(i, 1);
asserts.checkerror('[b,i]=sort([1,2],''bad'')', ...
  _('Sort direction must be either the string ''ascend'' or ''descend'''));
% Adjacent large integers must not be converted to a floating comparison key.
a = bitshift(uint64(1), 63) + uint64([2, 0, 1, 2, 0, 1]);
[b, i] = sort(a);
asserts.isequal(i, [2, 5, 3, 6, 1, 4]);
asserts.isequal(a, bitshift(uint64(1), 63) + uint64([2, 0, 1, 2, 0, 1]));
% Long missing groups expose partition-induced reordering.
a = repmat([NaN, 2, 1, NaN, 1, 2], 1, 10);
missingIndices = find(isnan(a));
onesIndices = find(a == 1);
twosIndices = find(a == 2);
[b, i] = sort(a, 'descend', 'MissingPlacement', 'first');
asserts.isequal(i, [missingIndices, twosIndices, onesIndices]);
[b, i] = sort(a, 'MissingPlacement', 'last');
asserts.isequal(i, [onesIndices, twosIndices, missingIndices]);
