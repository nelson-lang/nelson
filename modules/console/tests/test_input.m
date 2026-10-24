%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('input'), 1);
asserts.isequal(nargout('input'), 1);
%=============================================================================
asserts.checkerror('input();', _('Not enough input arguments.'));
asserts.checkerror('input(1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
asserts.checkerror('input(''prompt'', 1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 2, 'string'));
asserts.checkerror('input(''prompt'', ''x'');', _('Unrecognized option. "s" expected.'));
%=============================================================================
