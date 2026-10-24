%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('diag(1:3,1i)', _('Second argument must be a scalar.'));
asserts.checkerror('diag(1:3,1.5)', _('Second argument must be an integer scalar.'));
asserts.checkerror('diag(1:3,NaN)', _('Second argument must be an integer scalar.'));
asserts.checkerror('diag(1:3,Inf)', _('Second argument must be an integer scalar.'));
asserts.checkerror('diag(1:3,-Inf)', _('Second argument must be an integer scalar.'));
asserts.checkerror('diag(magic(3),1.5)', _('Second argument must be an integer scalar.'));
%=============================================================================
