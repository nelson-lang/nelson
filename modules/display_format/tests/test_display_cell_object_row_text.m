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
% a scalar or a row of duration, datetime, categorical or calendarDuration in a
% cell or a struct field shows its element texts; other shapes keep the size
% summary
%=============================================================================
format short
LF = char(10);
X = char(215);
%=============================================================================
C = {seconds(1), datetime(2020,1,1), categorical({'a'}), calendarDuration(1,2,3), table(1)};
REF = ['    {[1 sec]}    {[01-Jan-2020]}    {[a]}    {[1y 2mo 3d]}    {1', X, '1 table}', LF, LF];
asserts.isequal(evalc('disp(C)'), REF);
%=============================================================================
C = {hours([1 2]), [hours(1); hours(2)], seconds(zeros(1, 0))};
REF = ['    {[1 hr    2 hr]}    {2', X, '1 duration}    {1', X, '0 duration}', LF, LF];
asserts.isequal(evalc('disp(C)'), REF);
%=============================================================================
C = {datetime(2020,1,1:3)};
REF = ['    {[01-Jan-2020    02-Jan-2020    03-Jan-2020]}', LF, LF];
asserts.isequal(evalc('disp(C)'), REF);
%=============================================================================
C = {[seconds(NaN) seconds(2)], NaT, categorical({'a', 'bb', ''})};
REF = ['    {[NaN sec    2 sec]}    {[NaT]}    {[a    bb    <undefined>]}', LF, LF];
asserts.isequal(evalc('disp(C)'), REF);
%=============================================================================
C = {calendarDuration(1, 2, 3:4)};
REF = ['    {[1y 2mo 3d    1y 2mo 4d]}', LF, LF];
asserts.isequal(evalc('disp(C)'), REF);
%=============================================================================
% texts are left aligned inside the brackets of a column
C = {seconds(1), 'abc'; datetime(2020,1,1), 3};
REF = ['    {[1 sec      ]}    {''abc''}', LF, ...
       '    {[01-Jan-2020]}    {[  3]}', LF, LF];
asserts.isequal(evalc('disp(C)'), REF);
%=============================================================================
S = struct();
S.a = seconds([1 2]);
S.b = seconds(3);
S.c = datetime(2020,1,1);
S.d = categorical({'q', 'r'});
S.e = caldays([1 2]);
S.f = seconds([1; 2]);
REF = ['    a: [1 sec    2 sec]', LF, ...
       '    b: 3 sec', LF, ...
       '    c: 01-Jan-2020', LF, ...
       '    d: [q    r]', LF, ...
       '    e: [1d    2d]', LF, ...
       '    f: [2', X, '1 duration]', LF, LF];
asserts.isequal(evalc('disp(S)'), REF);
%=============================================================================
% a row too long for the terminal keeps the size summary
C = {seconds(1:200)};
asserts.isequal(evalc('disp(C)'), ['    {1', X, '200 duration}', LF, LF]);
%=============================================================================
% the element text method is hidden
assert_isfalse(any(strcmp(methods(seconds(1)), 'compactElementText')));
assert_isfalse(any(strcmp(methods(categorical({'a'})), 'compactElementText')));
%=============================================================================
