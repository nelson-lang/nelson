%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
dataInfo = tsdata.datametadata('Units', 'm', 'Interpolation', 'nearest');
asserts.isequal(dataInfo.Units, 'm');
asserts.isequal(dataInfo.Interpolation.Method, 'nearest');
dataInfo = set(dataInfo, 'Units', 'cm');
asserts.isequal(get(dataInfo, 'Units'), 'cm');
%=============================================================================
