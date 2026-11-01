%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
asserts.isequal(nargin('dbquit'), -1);
asserts.isequal(nargout('dbquit'), -1);
asserts.checkerror('dbquit', _('Debug commands only allowed when stopped in debug mode.'), 'Nelson:debugger:notInDebugMode');
%=============================================================================
