%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeInteger'), -1);
assert_isequal(nargout('mustBeInteger'), 0);
%=============================================================================
mustBeInteger([])
mustBeInteger([1, 2]);
assert_checkerror('mustBeInteger(NaN)', _('Value must be integer.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be integer.')];
assert_checkerror('mustBeInteger(1/3, 3)', msg);
%=============================================================================


