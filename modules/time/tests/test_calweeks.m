%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
t = datetime(2024, 1, 1) + calweeks(2);
asserts.isequal(day(t), 15);
t = datetime(2024, 1, 1) + calweeks([1 2]);
asserts.isequal(day(t), [8 15]);
asserts.istrue(iscalendarduration(calweeks(2)));
%=============================================================================
