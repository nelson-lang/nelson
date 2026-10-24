%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('bwselect(single([NaN 0]), 1, 1)', _('Conversion to logical with NaN is not possible.'));
%=============================================================================
asserts.checkerror('bwselect(logical([1 0]), [1 2], 1)', _('Selection coordinates must be numeric arrays of the same size.'));
%=============================================================================
asserts.checkerror('bwselect(logical([1 0]), 1, 1, 6)', _('Unsupported connectivity.'));
