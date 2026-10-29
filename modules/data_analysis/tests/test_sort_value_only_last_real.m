%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Every column has the same unordered values, with a different missing position.
a = [3, NaN, 2; 1, 2, NaN; 2, 1, 1; NaN, 3, 3];
ascending = [1, 1, 1; 2, 2, 2; 3, 3, 3; NaN, NaN, NaN];
descending = [3, 3, 3; 2, 2, 2; 1, 1, 1; NaN, NaN, NaN];
for type = {'double', 'single'}
  value = feval(type{1}, a);
  for direction = {'ascend', 'descend'}
    expected = ascending;
    if strcmp(direction{1}, 'descend'), expected = descending; end
    expected = feval(type{1}, expected);
    b = sort(value, 1, direction{1}, 'MissingPlacement', 'last');
    asserts.isequal(b, expected);
    [indexed, indices] = sort(value, 1, direction{1}, 'MissingPlacement', 'last');
    asserts.isequal(b, indexed);
    b = sort(value', 2, direction{1}, 'MissingPlacement', 'last');
    asserts.isequal(b, expected');
    b = sort(reshape([value, value], 4, 3, 2), 1, direction{1}, 'MissingPlacement', 'last');
    asserts.isequal(b, reshape([expected, expected], 4, 3, 2));
    % With no missing values the whole range must be sorted.
    b = sort(feval(type{1}, [3, 6; 1, 4; 2, 5]), 1, direction{1}, 'MissingPlacement', 'last');
    finiteExpected = [1, 4; 2, 5; 3, 6];
    if strcmp(direction{1}, 'descend'), finiteExpected = [3, 6; 2, 5; 1, 4]; end
    asserts.isequal(b, feval(type{1}, finiteExpected));
    b = sort(feval(type{1}, NaN(3, 2)), 1, direction{1}, 'MissingPlacement', 'last');
    asserts.isequal(b, feval(type{1}, NaN(3, 2)));
  end
end
% Explicitly verify scalar/vector/empty cases alongside the changed matrix path.
asserts.isequal(sort([NaN, 3, 1, 2, NaN], 'MissingPlacement', 'last'), [1, 2, 3, NaN, NaN]);
asserts.isequal(sort(zeros(0, 2), 'MissingPlacement', 'last'), zeros(0, 2));
asserts.isequal(sort(3, 'MissingPlacement', 'last'), 3);
asserts.checkerror('sort([1,2],''MissingPlacement'',''invalid'')', ...
  '''auto'', ''first'', or ''last'' for ''MissingPlacement'' parameter.');
