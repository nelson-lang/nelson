%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('webread'), -1);
asserts.isequal(nargout('webread'), -1);
%=============================================================================
asserts.checkerror('webread();', _('Not enough input arguments.'));
asserts.checkerror('webread(1);', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, '''char'' or ''string'''));
%=============================================================================
opts = weboptions();
asserts.checkerror('webread(''https://example.invalid'', opts, ''name'', ''value'');', ...
  _('weboptions must be last argument.'));
%=============================================================================
