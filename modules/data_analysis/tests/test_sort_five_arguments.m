%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Explicit dimension and direction precede the name/value option.
a = [3, 6; 1, 4; 2, 5];
expected = [1, 4; 2, 5; 3, 6];
for direction = {'ascend', 'descend'}
  order = [2; 3; 1];
  values = expected;
  if strcmp(direction{1}, 'descend'), order = [1; 3; 2]; values = [3, 6; 2, 5; 1, 4]; end
  % ComparisonMethod also exercises case5 independently of missing placement.
  b = sort(a, 1, direction{1}, 'ComparisonMethod', 'real');
  asserts.isequal(b, values);
  [b, i] = sort(a, 1, direction{1}, 'ComparisonMethod', 'real');
  asserts.isequal(b, values);
  asserts.isequal(i, [order, order]);
  [full, indices] = sort(a, 1, direction{1}, 'ComparisonMethod', 'real', 'MissingPlacement', 'last');
  asserts.isequal(b, full);
  asserts.isequal(i, indices);
  b = sort(a, 1, direction{1}, 'MissingPlacement', 'last');
  asserts.isequal(b, values);
  [b, i] = sort(a, 1, direction{1}, 'MissingPlacement', 'last');
  asserts.isequal(b, values);
  asserts.isequal(i, [order, order]);
end
% Four arguments omit the direction; seven make both options explicit.
b4 = sort(a, 1, 'MissingPlacement', 'last');
b5 = sort(a, 1, 'ascend', 'MissingPlacement', 'last');
b7 = sort(a, 1, 'ascend', 'MissingPlacement', 'last', 'ComparisonMethod', 'auto');
asserts.isequal(b4, expected);
asserts.isequal(b5, b4);
asserts.isequal(b7, b4);
% The text-key form with two name/value options keeps its existing parsing.
asserts.isequal(sort([-3, 1, -2], 'ComparisonMethod', 'abs', 'MissingPlacement', 'last'), [1, -2, -3]);
% Rejected names, values, direction and dimension must not be reinterpreted.
asserts.checkerror('sort(a,1,''ascend'',''unknown'',''last'')', ...
  '''ComparisonMethod'' or ''MissingPlacement'' expected.');
asserts.checkerror('sort(a,1,''ascend'',''MissingPlacement'',''invalid'')', ...
  '''auto'', ''first'', or ''last'' for ''MissingPlacement'' parameter.');
asserts.checkerror('sort(a,1,''ascend'',''ComparisonMethod'',''invalid'')', ...
  '''auto'', ''real'', or ''abs'' for ''ComparisonMethod'' parameter.');
asserts.checkerror('sort(a,1,''invalid'',''MissingPlacement'',''last'')', ...
  'Sort direction must be either the string ''ascend'' or ''descend''');
asserts.checkerror('sort(a,0,''ascend'',''MissingPlacement'',''last'')', ...
  'Dimension argument must be a positive integer scalar.');
% Count-based dispatch preserves all other existing forms from one to seven inputs.
a = [3, 6; 1, 4; 2, 5];
up = [1, 4; 2, 5; 3, 6];
down = [3, 6; 2, 5; 1, 4];
asserts.isequal(sort(a), up);
asserts.isequal(sort(a, 1), up);
asserts.isequal(sort(a, 'descend'), down);
asserts.isequal(sort(a, 1, 'descend'), down);
asserts.isequal(sort(a, 'MissingPlacement', 'last'), up);
asserts.isequal(sort(a, 'ComparisonMethod', 'real'), up);
asserts.isequal(sort(a, 'descend', 'MissingPlacement', 'last'), down);
asserts.isequal(sort(a, 1, 'ComparisonMethod', 'real'), up);
asserts.isequal(sort(a, 'descend', 'ComparisonMethod', 'real'), down);
asserts.isequal(sort(a, 'MissingPlacement', 'last', 'ComparisonMethod', 'real'), up);
asserts.isequal(sort(a, 'ComparisonMethod', 'real', 'MissingPlacement', 'last'), up);
asserts.isequal(sort(a, 1, 'MissingPlacement', 'last', 'ComparisonMethod', 'real'), up);
asserts.isequal(sort(a, 'descend', 'MissingPlacement', 'last', 'ComparisonMethod', 'real'), down);
asserts.isequal(sort(a, 1, 'ComparisonMethod', 'real', 'MissingPlacement', 'last'), up);
asserts.isequal(sort(a, 'descend', 'ComparisonMethod', 'real', 'MissingPlacement', 'last'), down);
asserts.isequal(sort(a, 1, 'descend', 'MissingPlacement', 'last', 'ComparisonMethod', 'real'), down);
% Existing short direction aliases and repeated options remain supported.
asserts.isequal(sort(a, 'd'), down);
asserts.isequal(sort(a, 1, 'a', 'MissingPlacement', 'last'), up);
asserts.isequal(sort(a, 'MissingPlacement', 'first', 'MissingPlacement', 'last'), up);
% Error precedence remains direction before options, and first option before second.
asserts.checkerror('sort(a,1,''invalid'',''unknown'',''invalid'')', ...
  'Sort direction must be either the string ''ascend'' or ''descend''');
asserts.checkerror('sort(a,''MissingPlacement'',''invalid'',''unknown'',''invalid'')', ...
  '''auto'', ''first'', or ''last'' for ''MissingPlacement'' parameter.');
asserts.checkerror('sort(a,1,''ascend'',''ComparisonMethod'',''invalid'',''unknown'',''invalid'')', ...
  '''auto'', ''real'', or ''abs'' for ''ComparisonMethod'' parameter.');
