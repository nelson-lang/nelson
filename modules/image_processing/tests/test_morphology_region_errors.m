%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imresize(zeros(2, 2, 2), 2)', sprintf(_('%s must be a 2-D or RGB array.'), 'Input image'));
%=============================================================================
asserts.checkerror('imrotate(zeros(2, 2, 2), 45)', sprintf(_('%s must be a 2-D, RGB, or RGBA array.'), 'Input image'));
%=============================================================================
asserts.checkerror('imrotate([1 + i 2], 45)', sprintf(_('%s must be real.'), 'Input image'));
%=============================================================================
asserts.checkerror('imrotate([1 2], 45, 1)', sprintf(_('%s must be a string.'), 'Interpolation method'));
%=============================================================================
asserts.checkerror('imrotate([1 2], 45, ''unknown'')', _('Interpolation method must be ''nearest'', ''bilinear'', or ''bicubic''.'));
%=============================================================================
asserts.checkerror('imrotate([1 2], 45, ''nearest'', 1)', sprintf(_('%s must be a string.'), 'Bounding box'));
%=============================================================================
asserts.checkerror('imrotate([1 2], 45, ''nearest'', ''unknown'')', _('Bounding box must be ''loose'' or ''crop''.'));
