%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('imfilter(sparse([1 2]), ones(1, 1))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('imfilter(zeros(2, 2, 2), ones(1, 1))', sprintf(_('%s must be a 2-D or RGB array.'), 'Image'));
%=============================================================================
asserts.checkerror('imfilter([1 2], {1})', _('Filter kernel must be a nonempty 2-D numeric real array.'));
%=============================================================================
asserts.checkerror('imfilter([1 2], 1 + i)', _('Filter kernel must be a nonempty 2-D numeric real array.'));
%=============================================================================
asserts.checkerror('imfilter([1 2], ones(1, 1), {})', sprintf(_('Unsupported "%s" option.'), 'imfilter'));
