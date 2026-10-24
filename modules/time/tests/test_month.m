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
t = datetime(2024, [1 12], [1 31]);
asserts.isequal(month(t), [1 12]);
asserts.isequal(month(datenum(2024, 5, 17)), 5);
asserts.isequal(month(datetime(2024, 1, 1), 'shortname'), "Jan");
asserts.isequal(month(datetime(2024, 12, 1), 'name'), "December");
%=============================================================================
