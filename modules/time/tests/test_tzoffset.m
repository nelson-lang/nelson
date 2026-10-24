%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
z = datetime(2024, 1, 1, 'TimeZone', '+02:30');
asserts.isequal(seconds(tzoffset(z)), 9000);
z = datetime(2024, 1, 1, 'TimeZone', 'Europe/Paris');
asserts.isequal(seconds(tzoffset(z)), 3600);
z = datetime(2024, 7, 1, 'TimeZone', 'Europe/Paris');
asserts.isequal(seconds(tzoffset(z)), 7200);
z = datetime(2024, 1, 1, 'TimeZone', 'America/New_York');
asserts.isequal(seconds(tzoffset(z)), -18000);
%=============================================================================
