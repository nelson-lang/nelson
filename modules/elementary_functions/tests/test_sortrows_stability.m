%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Successive keys must preserve ties above the short-sort threshold.
for n = [33, 60]
  a = [mod((1:n)', 3), (1:n)'];
  expected = [3:3:n, 1:3:n, 2:3:n]';
  [b, i] = sortrows(a);
  asserts.isequal(i, expected);
  asserts.isequal(b, a(expected, :));
  expected = [2:3:n, 1:3:n, 3:3:n]';
  [b, i] = sortrows(a, [-1, 2]);
  asserts.isequal(i, expected);
  asserts.isequal(b, a(expected, :));
  [b, i] = sortrows(a, [1, 2], {'descend', 'ascend'});
  asserts.isequal(i, expected);
end
% Numeric classes and duplicate full rows retain the original row order.
a = [2, 3; 1, 2; 2, 1; 1, 2; 2, 1; 1, 1];
for type = {'double', 'single', 'int32', 'uint64', 'char'}
  value = feval(type{1}, a);
  [b, i] = sortrows(value);
  asserts.isequal(i, [6; 2; 4; 3; 5; 1]);
  asserts.isequal(b, value(i, :));
end
% Descending cell keys reverse groups, not rows within a group.
a = {'a', 'b'; 'a', 'a'; 'b', 'a'; 'b', 'a'};
[b, i] = sortrows(a, [-1, 2]);
asserts.isequal(i, [3; 4; 2; 1]);
asserts.isequal(b, a(i, :));
[b, i] = sortrows(a, 1, 'descend');
asserts.isequal(i, [3; 4; 1; 2]);
[b, i] = sortrows(string(a), [-1, 2]);
asserts.isequal(i, [3; 4; 2; 1]);
% Missing values tie as keys and retain the order established by the next key.
a = [NaN, 3; 1, 2; NaN, 1; 1, 1; NaN, 1];
[b, i] = sortrows(a);
asserts.isequal(i, [4; 2; 3; 5; 1]);
[b, i] = sortrows(a, [-1, 2]);
asserts.isequal(i, [3; 5; 1; 4; 2]);
% Independent graph-edge reference, with duplicates from adjacent triangles.
t = [1, 3, 13; 3, 4, 13; 4, 5, 13; 5, 6, 13; 6, 2, 13; 2, 1, 13; ...
  3, 7, 14; 7, 8, 14; 8, 4, 14; 4, 3, 14; 5, 9, 15; 9, 10, 15; 10, 6, 15; 6, 5, 15; ...
  7, 11, 16; 11, 12, 16; 12, 10, 16; 10, 9, 16; 9, 8, 16; 8, 7, 16];
a = sort([t(:, [1, 2]); t(:, [2, 3]); t(:, [3, 1])], 2);
expected = zeros(0, 2);
for first = 1:16
  for second = first:16
    rows = find(a(:, 1) == first & a(:, 2) == second);
    expected = [expected; a(rows, :)];
  end
end
asserts.isequal(sortrows(a), expected);
asserts.checkerror('sortrows([1,2],0)', ...
  'Column indices must be nonzero integers not exceeding the number of columns.');
asserts.checkerror('sortrows([1,2],[1,2],{''ascend'',''ascend'',''ascend''})', ...
  'The number of directions must match the number of columns.');
% Long cell keys also retain the order established by secondary columns.
keys = {'a'; 'b'; 'c'};
a = keys(mod((1:60)', 3) + 1);
labels = cell(60, 1);
for k = 1:60, labels{k} = sprintf('%02d', k); end
value = [a, labels];
[b, i] = sortrows(value, [-1, 2]);
asserts.isequal(i, [2:3:60, 1:3:60, 3:3:60]');
asserts.isequal(b, value(i, :));
