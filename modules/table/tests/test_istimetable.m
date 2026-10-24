%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2]), [3; 4]);
T = table([1; 2]);
asserts.istrue(istimetable(TT));
asserts.isfalse(istimetable(T));
asserts.isfalse(istimetable([1; 2]));
%=============================================================================
