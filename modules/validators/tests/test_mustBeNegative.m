%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNegative'), -1);
assert_isequal(nargout('mustBeNegative'), 0);
%=============================================================================
mustBeNegative([]);
mustBeNegative([-1, -2]);
assert_checkerror('mustBeNegative(Inf)', _('Value must be negative.'), 'Nelson:validators:mustBeNegative');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be negative.')];
assert_checkerror('mustBeNegative(Inf, 3)', msg, 'Nelson:validators:mustBeNegative');
%=============================================================================
