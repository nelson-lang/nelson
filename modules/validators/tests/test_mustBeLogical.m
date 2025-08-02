%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeLogical'), -1);
assert_isequal(nargout('mustBeLogical'), 0);
%=============================================================================
mustBeLogical([]);
mustBeLogical([true, false]);
assert_checkerror('mustBeLogical(1)', _('Value must be logical.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be logical.')];
assert_checkerror('mustBeLogical(1, 3)', msg);
%=============================================================================
