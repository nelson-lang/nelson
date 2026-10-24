%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('websave'), -1);
asserts.isequal(nargout('websave'), 1);
%=============================================================================
asserts.checkerror('websave();', _('Not enough input arguments.'));
asserts.checkerror('websave(''target.txt'');', _('Not enough input arguments.'));
asserts.checkerror('websave(1, ''https://example.invalid'');', ...
  sprintf(_('Wrong type for argument #%d: %s expected.'), 1, '''char'' or ''string'''));
asserts.checkerror('websave(''target.txt'', 1);', ...
  sprintf(_('Wrong type for argument #%d: %s expected.'), 1, '''char'' or ''string'''));
%=============================================================================
opts = weboptions();
asserts.checkerror('websave(''target.txt'', ''https://example.invalid'', opts, ''name'', ''value'');', ...
  _('weboptions must be last argument.'));
%=============================================================================
