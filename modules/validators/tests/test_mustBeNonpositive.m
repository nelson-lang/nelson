%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNonpositive'), -1);
assert_isequal(nargout('mustBeNonpositive'), 0);
%=============================================================================
mustBeNonpositive([-1, -2]);
assert_checkerror('mustBeNonpositive(Inf)', _('Value must be non positive.'), 'Nelson:validators:mustBeNonpositive');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be non positive.')];
assert_checkerror('mustBeNonpositive(Inf, 3)', msg, 'Nelson:validators:mustBeNonpositive');
%=============================================================================
