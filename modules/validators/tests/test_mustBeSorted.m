%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeSorted'), -2);
asserts.isequal(nargout('mustBeSorted'), 0);
%=============================================================================
msg = sprintf(_('%s must be in sorted order.'), 'Value');
id = 'Nelson:validators:mustBeSorted';
%=============================================================================
% documented example
asserts.checkerror('mustBeSorted([5 3 3 1])', msg, id);
mustBeSorted([5 3 3 1], 'descend');
mustBeSorted([5 3 3 1], "descend");
%=============================================================================
% trivially sorted
mustBeSorted([]);
mustBeSorted(zeros(0, 3));
mustBeSorted(7);
mustBeSorted(NaN, 'strictascend');
mustBeSorted([], 'strictascend');
mustBeSorted([3 1], 3);
mustBeSorted(ones(2, 2, 2), 3);
%=============================================================================
% dim: matrix columns by default
mustBeSorted([1 2; 3 4]);
asserts.checkerror('mustBeSorted([1 2; 0 3])', msg, id);
mustBeSorted([1 2; 0 3], 2);
asserts.checkerror('mustBeSorted([1 2; 0 3], 1)', msg, id);
asserts.checkerror('mustBeSorted([1 2; 2 1], ''monotonic'')', msg, id);
asserts.checkerror('mustBeSorted([1 2; 0 3], ''monotonic'')', msg, id);
mustBeSorted(cat(3, [1 2], [3 4]), 3);
asserts.checkerror('mustBeSorted(cat(3, [1 2], [0 4]), 3)', msg, id);
%=============================================================================
% directions
mustBeSorted([1 1 2]);
mustBeSorted([2 2 1], 'descend');
mustBeSorted([2 2 1], 'monotonic');
mustBeSorted([1 1 2], 'monotonic');
mustBeSorted([1 1], 'monotonic');
mustBeSorted([1 2 3], 'strictascend');
mustBeSorted([3 2 1], 'strictdescend');
mustBeSorted([3 2 1], 'strictmonotonic');
mustBeSorted([1 2 3], 'StrictMonotonic');
asserts.checkerror('mustBeSorted([1 1 2], ''strictascend'')', msg, id);
asserts.checkerror('mustBeSorted([2 2 1], ''strictdescend'')', msg, id);
asserts.checkerror('mustBeSorted([1 1], ''strictmonotonic'')', msg, id);
asserts.checkerror('mustBeSorted([1 3 2], ''monotonic'')', msg, id);
asserts.checkerror('mustBeSorted([1 2], ''descend'')', msg, id);
%=============================================================================
% missing values
mustBeSorted([1 NaN]);
mustBeSorted([1 NaN NaN]);
mustBeSorted([1 2 NaN], 'MissingPlacement', 'last');
mustBeSorted(single([1 NaN]));
mustBeSorted([NaN 1], 'descend');
mustBeSorted([NaN 3 2], 'descend');
mustBeSorted([NaN 3 2], 'monotonic');
mustBeSorted([1 2 NaN], 'monotonic');
mustBeSorted([NaN 1 2], 'ascend', 'MissingPlacement', 'first');
mustBeSorted([NaN 1 2], MissingPlacement = "first");
mustBeSorted([NaN NaN 1], 'MissingPlacement', 'first');
mustBeSorted([NaN NaN 2 1], 'descend');
mustBeSorted([1 Inf]);
mustBeSorted([-Inf 1]);
asserts.checkerror('mustBeSorted([NaN 1])', msg, id);
asserts.checkerror('mustBeSorted([1 NaN], ''descend'')', msg, id);
asserts.checkerror('mustBeSorted([1 NaN 3])', msg, id);
asserts.checkerror('mustBeSorted([1 NaN], ''strictascend'')', msg, id);
asserts.checkerror('mustBeSorted([NaN NaN], ''strictascend'')', msg, id);
asserts.checkerror('mustBeSorted([3 2 NaN], ''descend'')', msg, id);
asserts.checkerror('mustBeSorted([NaN 1 2], ''monotonic'')', msg, id);
asserts.checkerror('mustBeSorted([2 1 NaN], ''monotonic'')', msg, id);
asserts.checkerror('mustBeSorted([1 NaN 2], ''monotonic'')', msg, id);
asserts.checkerror('mustBeSorted([1 2 NaN], ''ascend'', ''MissingPlacement'', ''first'')', msg, id);
asserts.checkerror('mustBeSorted([1 NaN 2], ''ascend'', ''MissingPlacement'', ''first'')', msg, id);
asserts.checkerror('mustBeSorted([NaN 1 2], ''strictascend'', ''MissingPlacement'', ''first'')', msg, id);
%=============================================================================
% integer, logical and char
mustBeSorted(int8([1 2 2]));
mustBeSorted(uint64([1 2 18446744073709551615]));
mustBeSorted([false true]);
mustBeSorted('abc');
asserts.checkerror('mustBeSorted(int8([1 2 2]), ''strictascend'')', msg, id);
asserts.checkerror('mustBeSorted([true false])', msg, id);
asserts.checkerror('mustBeSorted(''cba'')', msg, id);
%=============================================================================
% string and cell array of character vectors
mustBeSorted(["a", "b"]);
mustBeSorted(["a", missing]);
mustBeSorted({'a', 'b'});
mustBeSorted({'', 'a'});
mustBeSorted(["c"; "b"; "a"], 'descend');
mustBeSorted(["B", "a"]);
mustBeSorted({'B', 'a'});
mustBeSorted([missing, "a", "b"], 'MissingPlacement', 'first');
asserts.checkerror('mustBeSorted([missing, "b", "a"], ''strictdescend'')', msg, id);
asserts.checkerror('mustBeSorted(["b", "a"])', msg, id);
asserts.checkerror('mustBeSorted([missing, "a"])', msg, id);
asserts.checkerror('mustBeSorted({''b'', ''a''})', msg, id);
asserts.checkerror('mustBeSorted({''a'', '''', ''b''})', msg, id);
%=============================================================================
% complex values and ComparisonMethod
mustBeSorted([1 - 1i, 1 + 1i]);
mustBeSorted([1 - 1i, 1 + 1i], 'ComparisonMethod', 'real');
mustBeSorted([1 + 2i, NaN]);
mustBeSorted([1 -2 3], 'ComparisonMethod', 'abs');
mustBeSorted([1 -1], 'ComparisonMethod', 'abs');
mustBeSorted(int8([1 -2 3]), 'ComparisonMethod', 'abs');
asserts.checkerror('mustBeSorted([1+1i, 1-1i])', msg, id);
asserts.checkerror('mustBeSorted([1+1i, 1-1i], ''ComparisonMethod'', ''real'')', msg, id);
asserts.checkerror('mustBeSorted([NaN, 1+2i])', msg, id);
asserts.checkerror('mustBeSorted([1 -2 3])', msg, id);
asserts.checkerror('mustBeSorted([-1 1], ''ComparisonMethod'', ''abs'')', msg, id);
asserts.checkerror('mustBeSorted([1 1i], ''descend'')', msg, id);
%=============================================================================
% sparse
mustBeSorted(sparse([0 1 2]));
asserts.checkerror('mustBeSorted(sparse([1 2 0]))', msg, id);
%=============================================================================
% invalid options
asserts.checkerror('mustBeSorted([1 2], 0)', _('Dimension argument must be a positive integer scalar.'), 'Nelson:validators:invalidDimension');
asserts.checkerror('mustBeSorted([1 2], 1.5)', _('Dimension argument must be a positive integer scalar.'), 'Nelson:validators:invalidDimension');
asserts.checkerror('mustBeSorted([1 2], ''up'')', _('Sorting direction must be ''ascend'', ''descend'', ''monotonic'', ''strictascend'', ''strictdescend'' or ''strictmonotonic''.'), 'Nelson:validators:invalidSortDirection');
asserts.checkerror('mustBeSorted([1 2], ''MissingPlacement'', ''middle'')', _('MissingPlacement must be ''auto'', ''first'' or ''last''.'), 'Nelson:validators:invalidMissingPlacement');
asserts.checkerror('mustBeSorted([1 2], ''ComparisonMethod'', ''imag'')', _('ComparisonMethod must be ''auto'', ''real'' or ''abs''.'), 'Nelson:validators:invalidComparisonMethod');
asserts.checkerror('mustBeSorted([1 2], ''Foo'', ''auto'')', _('Name-value argument must be ''MissingPlacement'' or ''ComparisonMethod''.'), 'Nelson:validators:invalidNameValueArgument');
%=============================================================================
