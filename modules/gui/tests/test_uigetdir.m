%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
%=============================================================================
asserts.isequal(nargin('uigetdir'), -1);
asserts.isequal(nargout('uigetdir'), 1);
%=============================================================================
asserts.checkerror('[a, b] = uigetdir()', _('Too many output arguments.'));
asserts.checkerror('uigetdir(1)', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
asserts.checkerror('uigetdir(''.'', 1)', sprintf(_('Wrong type for argument #%d: %s expected.'), 2, 'string'));
%=============================================================================
