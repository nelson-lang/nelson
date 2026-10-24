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
asserts.isequal(nargin('uiconfirm'), -4);
asserts.isequal(nargout('uiconfirm'), 1);
%=============================================================================
asserts.checkerror('[a, b] = uiconfirm(1, ''message'', ''title'')', _('Too many output arguments.'));
asserts.checkerror('uiconfirm()', _('Not enough input arguments.'));
asserts.checkerror('uiconfirm(1, ''message'')', _('Not enough input arguments.'));
asserts.checkerror('uiconfirm(1, ''message'', ''title'')', _('Invalid or deleted figure handle. First argument must be a valid figure handle.'));
%=============================================================================
