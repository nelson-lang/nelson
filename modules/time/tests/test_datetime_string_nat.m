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
% string() of a datetime returns <missing> for NaT elements
s = string(datetime(2020, 1, [1 NaN]));
assert_isequal(class(s), 'string');
assert_isequal(s(1), "01-Jan-2020");
assert_isequal(ismissing(s), [false true]);
assert_istrue(ismissing(string(NaT)));
s = string(NaT(2, 2));
assert_isequal(size(s), [2 2]);
assert_istrue(all(ismissing(s(:))));
%=============================================================================
s = string(datetime(2020, 1, [1 NaN]), 'yyyy-MM-dd');
assert_isequal(s(1), "2020-01-01");
assert_isequal(ismissing(s), [false true]);
d = datetime(2020, 1, [1 NaN]);
d.Format = 'yyyy';
s = string(d);
assert_isequal(s(1), "2020");
assert_isequal(ismissing(s), [false true]);
%=============================================================================
% display and char keep the NaT text
assert_isequal(char(NaT), 'NaT');
R = evalc('disp(datetime(2020, 1, [1 NaN]))');
assert_istrue(contains(R, 'NaT'));
%=============================================================================
% missing assigned into a datetime gives NaT
d = datetime(2020, 1, 1);
d(2) = missing;
assert_isequal(size(d), [1 2]);
assert_isequal(isnat(d), [false true]);
d = datetime(2020, 1, [1 2]);
d(:) = missing;
assert_isequal(isnat(d), [true true]);
%=============================================================================
% missing converts to NaT
assert_istrue(isnat(datetime(missing)));
d = datetime(missing, 'Format', 'yyyy');
assert_istrue(isnat(d));
assert_isequal(d.Format, 'yyyy');
assert_istrue(isnat(datetime(missing, 'TimeZone', 'UTC')));
d = [datetime(2020, 1, 1), missing];
assert_isequal(isnat(d), [false true]);
d = [missing, datetime(2020, 1, 1)];
assert_isequal(isnat(d), [true false]);
d = [datetime(2020, 1, 1); missing];
assert_isequal(size(d), [2 1]);
assert_isequal(isnat(d), [false; true]);
%=============================================================================
