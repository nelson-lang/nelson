%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
t = datetime(2024, 1, 31) + calquarters(1);
asserts.isequal([year(t), month(t), day(t)], [2024 4 30]);
t = datetime(2024, 1, 1) + calquarters([1 2]);
asserts.isequal(month(t), [4 7]);
asserts.istrue(iscalendarduration(calquarters(2)));
%=============================================================================
