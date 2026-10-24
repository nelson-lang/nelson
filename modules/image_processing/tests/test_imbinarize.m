%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
I = [0 0.25 0.75 1; 0.1 0.4 0.6 0.9];
BW = imbinarize(I, 0.5);
asserts.isequal(BW, logical([0 0 1 1; 0 0 1 1]));
%=============================================================================
asserts.checkerror('imbinarize([1 2], ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'imbinarize method'));
%=============================================================================
asserts.checkerror('imbinarize([1 2], ''adaptive'', ''Unknown'', true)', sprintf(_('Unsupported "%s" option.'), 'imbinarize'));
%=============================================================================
asserts.checkerror('imbinarize([1 2], ''adaptive'', ''ForegroundPolarity'', ''unknown'')', sprintf(_('Unsupported "%s" option.'), 'foreground polarity'));
