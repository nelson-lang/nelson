%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% NaN, Inf and empty numbers concatenated with a duration
d = [seconds(5); NaN; hours(2)];
assert_isequal(class(d), 'duration');
assert_isequal(size(d), [3 1]);
assert_isequal(seconds(d), [5; NaN; 7200]);
%=============================================================================
d = [NaN; seconds(1)];
assert_isequal(seconds(d), [NaN; 1]);
%=============================================================================
d = [seconds(1), Inf, -Inf];
assert_isequal(seconds(d), [1, Inf, -Inf]);
%=============================================================================
d = [seconds(1); []];
assert_isequal(class(d), 'duration');
assert_isequal(seconds(d), 1);
d = [[], seconds(1)];
assert_isequal(seconds(d), 1);
%=============================================================================
d = horzcat(seconds(1), NaN);
assert_isequal(seconds(d), [1, NaN]);
d = vertcat(NaN, seconds(1));
assert_isequal(seconds(d), [NaN; 1]);
%=============================================================================
% cat keeps the requested dimension
d = cat(1, seconds(1), NaN);
assert_isequal(seconds(d), [1; NaN]);
d = cat(2, 2, seconds(1));
assert_isequal(seconds(d), [2 * 86400, 1]);
d = cat(3, seconds(1), seconds(2));
assert_isequal(class(d), 'duration');
assert_isequal(size(d), [1 1 2]);
assert_isequal(seconds(d), cat(3, 1, 2));
d = cat(3, seconds([1 2]), [3 4]);
assert_isequal(size(d), [1 2 2]);
assert_isequal(seconds(d), cat(3, [1 2], [3 4] * 86400));
d = cat(4, 2, seconds(1));
assert_isequal(size(d), [1 1 1 2]);
%=============================================================================
% logical values are days too
d = [true; seconds(1); false];
assert_isequal(seconds(d), [86400; 1; 0]);
%=============================================================================
assert_checkerror('[seconds([1 2]); NaN]', _('Dimensions of arrays being concatenated are not consistent.'));
%=============================================================================
