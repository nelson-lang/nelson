%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('odephas3(0, [1; 2], '''')', _('Not enough solution components.'));
asserts.checkerror('odephas3()', _('Not enough input arguments.'));
%=============================================================================
