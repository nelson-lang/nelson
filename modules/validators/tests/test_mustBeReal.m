%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeReal'), -1);
assert_isequal(nargout('mustBeReal'), 0);
%=============================================================================
mustBeNonZero(1);
assert_checkerror('mustBeReal(i)', _('Value must be real.'), 'Nelson:validators:mustBeReal');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be real.')];
assert_checkerror('mustBeReal(i, 3)', msg, 'Nelson:validators:mustBeReal');
%=============================================================================
