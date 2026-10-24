%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], 'VariableNames', {'A'});
TT = timetable(seconds([1; 2]), [10; 20], 'VariableNames', {'A'});
asserts.istrue(istable(T));
asserts.isfalse(istable(TT));
asserts.isfalse(istable([1; 2]));
%=============================================================================
