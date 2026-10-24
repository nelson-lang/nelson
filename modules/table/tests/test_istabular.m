%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2]);
TT = timetable(seconds([1; 2]), [3; 4]);
asserts.istrue(istabular(T));
asserts.istrue(istabular(TT));
asserts.isfalse(istabular([1; 2]));
%=============================================================================
