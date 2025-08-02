%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeNonmissing'), -1);
assert_isequal(nargout('mustBeNonmissing'), 0);
%=============================================================================
mustBeNonmissing([]);
mustBeNonmissing([true, false]);
assert_checkerror('mustBeNonmissing(["hello" string(NaN)])', _('Value must be non missing.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be non missing.')];
assert_checkerror('mustBeNonmissing(NaN, 3)', msg);
%=============================================================================
