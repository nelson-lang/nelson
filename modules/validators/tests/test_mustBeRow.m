%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeRow'), -1);
assert_isequal(nargout('mustBeRow'), 0);
%=============================================================================
mustBeRow('Nelson');
mustBeRow([1, 2, 3]);
%=============================================================================
assert_checkerror('mustBeRow([])', _('Value must be a row vector.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be a row vector.')];
assert_checkerror('mustBeRow([], 3)', msg);
%=============================================================================

