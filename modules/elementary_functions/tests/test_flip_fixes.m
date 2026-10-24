%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('flip(1:3,1.5)', _('Dimension argument must be a positive integer scalar.'));
asserts.checkerror('flip(1:3,''a'')', _('Dimension argument must be a positive integer scalar.'));
asserts.checkerror('flip(1:3,NaN)', _('Dimension argument must be a positive integer scalar.'));
asserts.checkerror('flip(1:3,Inf)', _('Dimension argument must be a positive integer scalar.'));
%=============================================================================
