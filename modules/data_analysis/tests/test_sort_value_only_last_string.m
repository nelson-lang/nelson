%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A missing string belongs after an empty string, regardless of input order.
a = string({'z', '', 'a'});
a(1) = NaN;
b = sort(a, 'descend', 'MissingPlacement', 'last');
asserts.isequal(b, a([3, 2, 1]));
a = ["", NaN, "b", "a", NaN, "", "b"];
for direction = {'ascend', 'descend'}
  indices = [1, 6, 4, 3, 7, 2, 5];
  if strcmp(direction{1}, 'descend'), indices = [3, 7, 4, 1, 6, 2, 5]; end
  expected = a(indices);
  b = sort(a, direction{1}, 'MissingPlacement', 'last');
  asserts.isequal(b, expected);
  [indexed, positions] = sort(a, direction{1}, 'MissingPlacement', 'last');
  asserts.isequal(b, indexed);
  b = sort([a', a'], 1, direction{1}, 'MissingPlacement', 'last');
  asserts.isequal(b, [expected', expected']);
  b = sort([a; a], 2, direction{1}, 'MissingPlacement', 'last');
  asserts.isequal(b, [expected; expected]);
  nd = cat(3, [a; a], [a; a]);
  b = sort(nd, 2, direction{1}, 'MissingPlacement', 'last');
  asserts.isequal(b, cat(3, [expected; expected], [expected; expected]));
  onlyMissing = ["x", NaN, NaN];
  onlyMissing(1) = NaN;
  asserts.isequal(sort(onlyMissing, direction{1}, 'MissingPlacement', 'last'), onlyMissing);
end
% The finite-only and all-empty cases remain ordinary lexical sorts.
asserts.isequal(sort(["z", "a", ""], 'descend', 'MissingPlacement', 'last'), ["z", "a", ""]);
asserts.isequal(sort(["", "", ""], 'descend', 'MissingPlacement', 'last'), ["", "", ""]);
