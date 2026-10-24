%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% size(T) as a statement sets and displays ans.
%=============================================================================
T = table([1; 2]);
clear('ans');
printed = evalc('size(T)');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, [2 1]);
clear('ans');
printed = evalc('size(T, 2)');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, 1);
%=============================================================================
TT = timetable(seconds([1; 2]), [1; 2]);
clear('ans');
printed = evalc('size(TT)');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, [2 1]);
clear('ans');
printed = evalc('size(TT, 1)');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, 2);
%=============================================================================
clear('ans');
printed = evalc('height(TT)');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, 2);
clear('ans');
printed = evalc('width(TT)');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(ans, 1);
%=============================================================================
