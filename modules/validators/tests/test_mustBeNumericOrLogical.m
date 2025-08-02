%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNumericOrLogical'), -1);
assert_isequal(nargout('mustBeNumericOrLogical'), 0);
%=============================================================================
mustBeNumericOrLogical(1);
assert_checkerror('mustBeNumericOrLogical(struct())', _('Value must be numeric or logical.'), 'Nelson:validators:mustBeNumericOrLogical');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be numeric or logical.')];
assert_checkerror('mustBeNumericOrLogical(struct(), 3)', msg, 'Nelson:validators:mustBeNumericOrLogical');
%=============================================================================
