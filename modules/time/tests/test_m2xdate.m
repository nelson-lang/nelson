%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(m2xdate(datetime(1899, 12, 31)), 0);
asserts.isequal(m2xdate(datetime(1900, 1, 1)), 1);
asserts.isequal(m2xdate([datetime(1900, 1, 1), datetime(1900, 1, 2)]), [1 2]);
%=============================================================================
