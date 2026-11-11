%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% elapsed time between consecutive instants, across the DST fall-back
t = datetime(2024, 10, 27, 0, 30, 0, 'TimeZone', 'Europe/Paris') + hours([0 1 2 3 5]);
d = diff(t);
asserts.isequal(class(d), 'duration');
asserts.isequal(d.Format, 'hh:mm:ss');
asserts.isequal(hours(d), [1 1 1 2]);
asserts.isequal(hours(diff(t, 2)), [0 0 1]);
asserts.isequal(hours(diff(t, 3)), [0 1]);
asserts.isequal(size(diff(t, 6)), [1 0]);
asserts.isequal(hours(diff(t', 1, 1)), [1; 1; 1; 2]);
asserts.isequal(hours(diff(t(1:2), 1, 2)), 1);
asserts.isequal(evalc('disp(d)'), sprintf('   01:00:00   01:00:00   01:00:00   02:00:00\n\n'));
%=============================================================================
u = datetime(2024, 1, [1 3 10]);
asserts.isequal(hours(diff(u)), [48 168]);
asserts.isequal(hours(diff(datetime(2024, 1, 1) + calmonths(0:2))), [744 696]);
asserts.isequal(seconds(diff(datetime(2024, 1, 1, 0, 0, [0 0.5 1.25]))), [0.5 0.75]);
%=============================================================================
m = reshape(datetime(2024, 1, 1:6), 2, 3);
asserts.isequal(hours(diff(m)), [24 24 24]);
asserts.isequal(hours(diff(m, 1, 2)), [48 48; 48 48]);
asserts.isequal(size(diff(m, 1, 3)), [2 3 0]);
%=============================================================================
asserts.isequal(size(diff(datetime(2024, 1, 1))), [0 0]);
asserts.isequal(class(diff(datetime(2024, 1, 1))), 'duration');
asserts.isequal(size(diff(datetime.empty(0, 1))), [0 1]);
asserts.istrue(all(isnan(diff([datetime(2024, 1, 1) NaT datetime(2024, 1, 3)]))));
%=============================================================================
msg = sprintf(_('%s must be a positive integer scalar.'), 'Difference order N');
asserts.checkerror('diff(t, 0)', msg, 'Nelson:diff:differenceOrderMustBePositiveInteger');
asserts.checkerror('diff(t, 1.5)', msg, 'Nelson:diff:differenceOrderMustBePositiveInteger');
msg = _('Dimension argument must be a positive integer scalar within indexing range.');
asserts.checkerror('diff(t, 1, 0)', msg, 'Nelson:getdimarg:dimensionMustBePositiveInteger');
%=============================================================================
% durations keep their format
s = diff(seconds([1 2 4]));
asserts.isequal(seconds(s), [1 2]);
asserts.isequal(s.Format, 's');
asserts.isequal(diff(minutes([1 3 4])).Format, 'm');
asserts.isequal(size(diff(seconds(5))), [0 0]);
%=============================================================================
