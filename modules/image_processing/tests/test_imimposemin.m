%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imimposemin({1}, true)', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('imimposemin([1 + i], true)', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('imimposemin([1 Inf], logical([1 0]))', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('imimposemin([1 2], logical([1; 0]))', sprintf(_('%s must have the same size as %s.'), 'Marker mask', 'the image'));
%=============================================================================
asserts.checkerror('imimposemin([1 2], [1 + i 0])', sprintf(_('%s must be real.'), 'Marker mask'));
%=============================================================================
asserts.checkerror('imimposemin([1 2], [1 2])', sprintf(_('%s must contain binary values.'), 'Marker mask'));
%=============================================================================
asserts.checkerror('imimposemin([1 2], sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse marker masks'));
%=============================================================================
asserts.checkerror('imimposemin([1 2], logical([1 0]), eye(3))', _('Unsupported connectivity.'));
