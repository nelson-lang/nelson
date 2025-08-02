%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNonZero'), -1);
assert_isequal(nargout('mustBeNonZero'), 0);
%=============================================================================
mustBeNonZero(1);
assert_checkerror('mustBeNonZero(0)', _('Value must not be zero.'), 'Nelson:validators:mustBeNonZero');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must not be zero.')];
assert_checkerror('mustBeNonZero(0, 3)', msg, 'Nelson:validators:mustBeNonZero');
%=============================================================================
