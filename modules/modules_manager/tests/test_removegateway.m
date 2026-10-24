%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('removegateway'), 1);
asserts.isequal(nargout('removegateway'), 0);
%=============================================================================
asserts.checkerror('removegateway(1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
%=============================================================================
