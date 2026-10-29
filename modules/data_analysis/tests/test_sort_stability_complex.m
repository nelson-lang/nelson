%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Real/absolute comparisons, duplicate values and missing components.
a = [complex(2, 3), complex(2, 1), complex(2, -1), complex(2, 3), ...
  complex(NaN, 2), complex(3, NaN), complex(1, 0)];
for type = {'double', 'single'}
  value = feval(type{1}, a);
  for method = {'auto', 'real', 'abs'}
    for direction = {'ascend', 'descend'}
      for placement = {'auto', 'first', 'last'}
        expected = [7, 3, 2, 1, 4];
        if strcmp(direction{1}, 'descend'), expected = [1, 4, 2, 3, 7]; end
        missingFirst = strcmp(placement{1}, 'first') || ...
          (strcmp(placement{1}, 'auto') && strcmp(direction{1}, 'descend'));
        if missingFirst, expected = [5, 6, expected]; else, expected = [expected, 5, 6]; end
        [b, i] = sort(value, direction{1}, 'ComparisonMethod', method{1}, ...
          'MissingPlacement', placement{1});
        asserts.isequal(i, expected);
        asserts.isequal(b, value(expected));
      end
    end
  end
end
% Distinct real and magnitude orders, with phase as the magnitude tie-breaker.
a = [3, 2i, -3, -2i, 3, 2i, -3, -2i];
[b, i] = sort(a, 'ComparisonMethod', 'real');
asserts.isequal(i, [3, 7, 4, 8, 2, 6, 1, 5]);
[b, i] = sort(a, 'ComparisonMethod', 'abs');
asserts.isequal(i, [4, 8, 2, 6, 1, 5, 3, 7]);
% Long equivalent groups, dimensions and repeated missing values.
a = complex(mod((1:60)', 3), 1);
expected = [3:3:60, 1:3:60, 2:3:60]';
[b, i] = sort(a);
asserts.isequal(i, expected);
asserts.isequal(b, a(expected));
a = [a'; a'];
[b, i] = sort(a, 2);
asserts.isequal(i, repmat(expected', 2, 1));
asserts.isequal(b, a(:, expected));
a = repmat(complex(NaN, 1), 1, 60);
[b, i] = sort(a, 'descend', 'MissingPlacement', 'last');
asserts.isequal(i, 1:60);
asserts.isequal(b, a);
% Real values routed through the absolute-comparison implementation.
a = [-2, 2, -1, 1, -2, 2, -1, 1];
[b, i] = sort(a, 'ComparisonMethod', 'abs');
asserts.isequal(i, [4, 8, 3, 7, 2, 6, 1, 5]);
asserts.isequal(b, a(i));
