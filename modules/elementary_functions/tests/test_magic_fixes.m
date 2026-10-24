%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('magic(NaN)', _('Input must be a finite real scalar.'));
asserts.checkerror('magic(Inf)', _('Input must be a finite real scalar.'));
asserts.checkerror('magic([2 3])', _('Input must be a finite real scalar.'));
asserts.checkerror('magic(complex(NaN,1))', _('Input must be a finite real scalar.'));
%=============================================================================
