%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [10; 30; 20], 'VariableNames', {'A'});
[Top, Idx] = topkrows(TT, 2, 'A', 'descend');
asserts.isequal(Top.A, [30; 20]);
asserts.isequal(Idx, [2; 3]);
%=============================================================================
