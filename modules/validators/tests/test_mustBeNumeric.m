%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNumeric'), -1);
assert_isequal(nargout('mustBeNumeric'), 0);
%=============================================================================
mustBeNumeric([])
mustBeNumeric([1, 2]);
assert_checkerror('mustBeNumeric({1})', _('Value must be numeric.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be numeric.')];
assert_checkerror('mustBeNumeric({}, 3)', msg);
%=============================================================================


