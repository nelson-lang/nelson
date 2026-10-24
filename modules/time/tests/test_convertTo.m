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
t = datetime(1970, 1, 2);
asserts.isequal(convertTo(t, 'posixtime'), 86400);
asserts.isequal(convertTo(datetime(2024, 5, 17), 'yyyymmdd'), 20240517);
asserts.isequal(convertTo(datetime(1899, 12, 31), 'exceltime'), 0);
asserts.isequal(convertTo(t, 'datenum'), datenum(t));
%=============================================================================
