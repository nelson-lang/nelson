%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeColumn'), -1);
assert_isequal(nargout('mustBeColumn'), 0);
%=============================================================================
mustBeColumn(['N';'e';'l';'s';'o';'n']);
mustBeColumn([1; 2; 3]);
%=============================================================================
assert_checkerror('mustBeColumn([])', _('Value must be a column vector.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be a column vector.')];
assert_checkerror('mustBeColumn([], 3)', msg);
%=============================================================================

