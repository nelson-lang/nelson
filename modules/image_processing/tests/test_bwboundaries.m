%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('bwboundaries(logical([1 0]), 6)', _('Unsupported connectivity.'));
%=============================================================================
asserts.checkerror('bwboundaries(logical([1 0]), ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'boundary'));
%=============================================================================
asserts.checkerror('bwboundaries(single([NaN 0]))', _('Conversion to logical with NaN is not possible.'));
