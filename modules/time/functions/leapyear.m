%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = leapyear(year)
    mustBeNumeric(year, 1)
    mustBeReal(year, 1)
    year = floor(year);
    tf = ((mod(year, 4) == 0 & mod(year, 100) ~= 0) | mod(year, 400) == 0);
end
%=============================================================================
