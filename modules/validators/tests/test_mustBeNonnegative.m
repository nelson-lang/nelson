%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNonnegative'), -1);
assert_isequal(nargout('mustBeNonnegative'), 0);
%=============================================================================
mustBePositive([1, 2]);
assert_checkerror('mustBeNonnegative(-Inf)', _('Value must be nonnegative.'), 'Nelson:validators:mustBeNonnegative');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be nonnegative.')];
assert_checkerror('mustBeNonnegative(-Inf, 3)', msg, 'Nelson:validators:mustBeNonnegative');
%=============================================================================
