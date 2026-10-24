%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [10; 20; 30]);
asserts.istrue(issorted(TT));
asserts.isfalse(issorted(TT, 'descend'));
TT = timetable(seconds([3; 2; 1]), [30; 20; 10]);
asserts.isfalse(issorted(TT));
asserts.istrue(issorted(TT, 'descend'));
%=============================================================================
