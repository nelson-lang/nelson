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
asserts.isequal(isdst(datetime(2024, 1, 1, 'TimeZone', 'Europe/Paris')), false);
asserts.isequal(isdst(datetime(2024, 7, 1, 'TimeZone', 'Europe/Paris')), true);
asserts.isequal(isdst(datetime(2024, 7, 1, 'TimeZone', '+02:00')), false);
%=============================================================================
