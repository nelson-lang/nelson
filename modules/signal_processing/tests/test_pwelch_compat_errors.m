%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('pwelch(1:4, hamming(8))', _('Signal length must be at least the window length.'));
asserts.checkerror('pwelch(1:16, hamming(8), 8)', _('Overlap length must be less than window length.'));
%=============================================================================
