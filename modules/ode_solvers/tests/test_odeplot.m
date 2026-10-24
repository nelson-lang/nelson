%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('odeplot(0, 1, ''bad'')', _('Unknown output callback flag.'));
asserts.checkerror('odeplot()', _('Not enough input arguments.'));
%=============================================================================
