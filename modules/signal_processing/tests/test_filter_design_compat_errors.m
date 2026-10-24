%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('butter(0, 0.4)', _('Value must be positive.'));
asserts.checkerror('butter(3, [0.1 0.2 0.3])', _('Cutoff frequency must be a scalar or a two-element vector.'));
asserts.checkerror('fir2(4, [0 1], [1])', _('Frequency and magnitude vectors must have the same length.'));
%=============================================================================
