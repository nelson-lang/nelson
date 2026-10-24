%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('fitgeotrans([0 0; 1 0], [0 0; 1 0], ''affine'')', _('At least three point pairs are required for an affine transform.'));
%=============================================================================
asserts.checkerror('fitgeotrans([0 0; 1 0; 2 0], [0 0; 1 0; 2 0], ''affine'')', _('Control points do not define a unique transformation.'));
%=============================================================================
asserts.checkerror('fitgeotrans([0 0; 1 0; 0 1], [0 0; 1 0; 0 1], ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'geometric transformation type'));
%=============================================================================
asserts.checkerror('fitgeotrans([0 0; 1 0; 0 1], [0 0; 1 0; 0 1], 1)', sprintf(_('%s must be a string.'), 'Transformation type'));
