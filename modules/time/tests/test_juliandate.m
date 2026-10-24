%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(juliandate(datetime(2000, 1, 1)), datenum(2000, 1, 1) + 1721058.5);
asserts.isequal(juliandate(datetime(1970, 1, 1)), datenum(1970, 1, 1) + 1721058.5);
asserts.isequal(size(juliandate([datetime(2024, 1, 1), datetime(2024, 1, 2)])), [1 2]);
%=============================================================================
