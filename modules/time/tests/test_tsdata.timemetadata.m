%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
timeInfo = tsdata.timemetadata('Units', 'seconds', 'StartDate', '01-Jan-2024');
timeInfo = setTime(timeInfo, [0; 1; 2]);
asserts.isequal(timeInfo.Length, 3);
asserts.isequal(timeInfo.Start, 0);
asserts.isequal(timeInfo.End, 2);
asserts.isequal(getData(timeInfo), [0; 1; 2]);
%=============================================================================
