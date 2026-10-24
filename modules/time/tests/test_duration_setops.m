%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
%=============================================================================
% unique on a duration array: count, values and class.
du = [hours(1) hours(1) hours(2)];
C = unique(du);
assert_isequal(class(C), 'duration');
assert_isequal(numel(C), 2);
assert_isequal(C, [hours(1) hours(2)]);
%=============================================================================
% unique outputs [C, ia, ic] reconstruct the input.
du = [hours(3) hours(1) hours(1) hours(2)];
[C, ia, ic] = unique(du);
assert_isequal(C, [hours(1) hours(2) hours(3)]);
assert_isequal(ia, [2; 4; 1]);
assert_isequal(ic, [3; 1; 1; 2]);
assert_istrue(isequal(C(ic), du));
assert_istrue(isequal(C, du(ia)));
%=============================================================================
% unique 'stable'.
du = [hours(3) hours(1) hours(1) hours(2)];
C = unique(du, 'stable');
assert_isequal(C, [hours(3) hours(1) hours(2)]);
%=============================================================================
% unique preserves the Format.
du = [minutes(90) minutes(90) minutes(30)];
du.Format = 'hh:mm';
C = unique(du);
assert_isequal(C.Format, 'hh:mm');
%=============================================================================
% unique keeps each NaN duration distinct.
dun = [seconds(1) seconds(NaN) seconds(NaN)];
assert_isequal(numel(unique(dun)), 3);
%=============================================================================
% Column orientation is preserved.
dc = [hours(2); hours(1); hours(1)];
assert_isequal(size(unique(dc)), [2 1]);
%=============================================================================
% ismember scalar and vector, with location.
du = [hours(1) hours(2)];
assert_istrue(ismember(hours(2), du));
assert_isfalse(ismember(hours(3), du));
[tf, loc] = ismember([hours(2) hours(3)], du);
assert_isequal(tf, [true false]);
assert_isequal(loc, [2 0]);
%=============================================================================
% intersect / union / setdiff on duration.
c = intersect([hours(1) hours(2)], hours(2));
assert_isequal(class(c), 'duration');
assert_isequal(c, hours(2));
u = union(hours(1), hours(3));
assert_isequal(class(u), 'duration');
assert_isequal(u, [hours(1) hours(3)]);
[s, ia] = setdiff([hours(3) hours(1) hours(2)], hours(2));
assert_isequal(class(s), 'duration');
assert_isequal(s, [hours(1) hours(3)]);
assert_isequal(ia, [2; 1]);
%=============================================================================
% Format preserved through intersect/union/setdiff.
du = [minutes(90) minutes(30)];
du.Format = 'hh:mm';
assert_isequal(intersect(du, minutes(90)).Format, 'hh:mm');
assert_isequal(union(du, minutes(15)).Format, 'hh:mm');
assert_isequal(setdiff(du, minutes(90)).Format, 'hh:mm');
%=============================================================================
