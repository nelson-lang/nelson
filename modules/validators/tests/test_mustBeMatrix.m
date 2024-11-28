%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeMatrix'), -1);
assert_isequal(nargout('mustBeMatrix'), 0);
%=============================================================================
mustBeMatrix([]);
mustBeMatrix(single(3));
%=============================================================================
assert_checkerror('mustBeMatrix(ones(2, 3, 2))', _('Value must be a matrix.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be a matrix.')];
assert_checkerror('mustBeMatrix(ones(2, 3, 2), 3)', msg);
%=============================================================================

