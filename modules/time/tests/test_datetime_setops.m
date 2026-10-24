%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
%=============================================================================
% unique on a datetime array: count, values and class.
d = [datetime(2020, 1, 1) datetime(2020, 1, 1) datetime(2020, 2, 1)];
C = unique(d);
assert_isequal(class(C), 'datetime');
assert_isequal(numel(C), 2);
assert_isequal(C, [datetime(2020, 1, 1) datetime(2020, 2, 1)]);
%=============================================================================
% unique outputs [C, ia, ic] reconstruct the input.
d = [datetime(2020, 3, 1) datetime(2020, 1, 1) datetime(2020, 1, 1) datetime(2020, 2, 1)];
[C, ia, ic] = unique(d);
assert_isequal(C, [datetime(2020, 1, 1) datetime(2020, 2, 1) datetime(2020, 3, 1)]);
assert_isequal(ia, [2; 4; 1]);
assert_isequal(ic, [3; 1; 1; 2]);
assert_istrue(isequal(C(ic), d));
assert_istrue(isequal(C, d(ia)));
%=============================================================================
% unique 'stable' keeps first-occurrence order.
d = [datetime(2020, 3, 1) datetime(2020, 1, 1) datetime(2020, 1, 1) datetime(2020, 2, 1)];
C = unique(d, 'stable');
assert_isequal(C, [datetime(2020, 3, 1) datetime(2020, 1, 1) datetime(2020, 2, 1)]);
%=============================================================================
% unique preserves Format and TimeZone.
d = datetime(2020, 1, 1, 'TimeZone', 'UTC', 'Format', 'yyyy-MM-dd');
d = [d d d + days(1)];
C = unique(d);
assert_isequal(C.TimeZone, 'UTC');
assert_isequal(C.Format, 'yyyy-MM-dd');
%=============================================================================
% unique keeps each NaT distinct (matching numeric NaN behavior).
dn = [datetime(2020, 1, 1) NaT NaT datetime(2020, 1, 1)];
u = unique(dn);
assert_isequal(numel(u), 3);
%=============================================================================
% Column orientation is preserved.
dc = [datetime(2020, 2, 1); datetime(2020, 1, 1); datetime(2020, 1, 1)];
uc = unique(dc);
assert_isequal(size(uc), [2 1]);
%=============================================================================
% unique 'rows'.
M = [datetime(2020, 1, 1) datetime(2020, 2, 1); ...
     datetime(2020, 1, 1) datetime(2020, 2, 1); ...
     datetime(2021, 1, 1) datetime(2021, 2, 1)];
R = unique(M, 'rows');
assert_isequal(size(R), [2 2]);
assert_isequal(R, [datetime(2020, 1, 1) datetime(2020, 2, 1); ...
                   datetime(2021, 1, 1) datetime(2021, 2, 1)]);
%=============================================================================
% ismember scalar and vector, with location.
d = [datetime(2020, 1, 1) datetime(2020, 1, 1) datetime(2020, 2, 1)];
assert_istrue(ismember(datetime(2020, 1, 1), d));
assert_isfalse(ismember(datetime(2020, 3, 1), d));
[tf, loc] = ismember([datetime(2020, 2, 1) datetime(2020, 3, 1)], d);
assert_isequal(tf, [true false]);
assert_isequal(loc, [3 0]);
% NaT never matches.
assert_isfalse(ismember(NaT, d));
%=============================================================================
% intersect on datetime, with indices.
a = [datetime(2020, 1, 1) datetime(2020, 2, 1) datetime(2020, 3, 1)];
b = [datetime(2020, 2, 1) datetime(2020, 4, 1)];
[c, ia, ib] = intersect(a, b);
assert_isequal(class(c), 'datetime');
assert_isequal(c, datetime(2020, 2, 1));
assert_isequal(ia, 2);
assert_isequal(ib, 1);
assert_isequal(intersect(d, datetime(2020, 1, 1)), datetime(2020, 1, 1));
%=============================================================================
% union on datetime.
u = union(datetime(2020, 1, 1), datetime(2020, 3, 1));
assert_isequal(class(u), 'datetime');
assert_isequal(u, [datetime(2020, 1, 1) datetime(2020, 3, 1)]);
%=============================================================================
% setdiff on datetime.
s = setdiff(d, datetime(2020, 1, 1));
assert_isequal(class(s), 'datetime');
assert_isequal(s, datetime(2020, 2, 1));
%=============================================================================
% intersect/union/setdiff preserve Format and TimeZone.
a = datetime(2020, 1, 1, 'TimeZone', 'UTC', 'Format', 'yyyy-MM-dd');
a = [a a + days(1)];
b = a(1);
assert_isequal(intersect(a, b).TimeZone, 'UTC');
assert_isequal(intersect(a, b).Format, 'yyyy-MM-dd');
assert_isequal(union(a, b).TimeZone, 'UTC');
assert_isequal(setdiff(a, b).Format, 'yyyy-MM-dd');
%=============================================================================
% Sub-second precision survives unique (via the precise time of day).
t = datetime(1900, 1, 1, 0, 0, 0.123456);
u = unique([t t datetime(2000, 1, 1)]);
assert_isapprox(second(u(1)), 0.123456, 1e-9);
%=============================================================================
