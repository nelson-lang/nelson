%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A missing string and an empty string are different sorting groups.
a = ["b", "", NaN, "a", "b", NaN, ""];
for direction = {'ascend', 'descend'}
  for placement = {'auto', 'first', 'last'}
    expected = [2, 7, 4, 1, 5];
    if strcmp(direction{1}, 'descend'), expected = [1, 5, 4, 2, 7]; end
    missingFirst = strcmp(placement{1}, 'first') || ...
      (strcmp(placement{1}, 'auto') && strcmp(direction{1}, 'descend'));
    if missingFirst, expected = [3, 6, expected]; else, expected = [expected, 3, 6]; end
    [b, i] = sort(a, direction{1}, 'MissingPlacement', placement{1});
    asserts.isequal(i, expected);
    asserts.isequal(b, a(expected));
  end
end
keys = {'a'; 'b'; 'c'};
indices = mod((1:60)', 3) + 1;
a = keys(indices);
expected = [3:3:60, 1:3:60, 2:3:60]';
[b, i] = sort(a);
asserts.isequal(i, expected);
asserts.isequal(b, a(expected));
[b, i] = sort(a');
asserts.isequal(i, expected');
asserts.isequal(b, a(expected)');
% Columns have independent permutations and equal keys keep their own order.
a = {'z', 'b'; 'a', 'b'; 'z', 'a'; 'a', 'a'};
[b, i] = sort(a);
asserts.isequal(i, [2, 3; 4, 4; 1, 1; 3, 2]);
asserts.isequal(b, {'a', 'a'; 'a', 'a'; 'z', 'b'; 'z', 'b'});
value = string(keys(indices));
[b, i] = sort(value);
asserts.isequal(i, expected);
[b, i] = sort([value'; value'], 2, 'descend');
asserts.isequal(i, repmat([2:3:60, 1:3:60, 3:3:60], 2, 1));
asserts.checkerror('[b,i]=sort({''a'',''b''},1)', ...
  _('Only one input parameter is supported for cell arrays.'));
