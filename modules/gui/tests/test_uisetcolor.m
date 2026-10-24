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
asserts.isequal(nargin('uisetcolor'), -1);
asserts.isequal(nargout('uisetcolor'), 1);
%=============================================================================
asserts.checkerror('[a, b] = uisetcolor()', _('Too many output arguments.'));
asserts.checkerror('uisetcolor([1 0 0], ''Title'', ''extra'')', _('Too many input arguments.'));
%=============================================================================
