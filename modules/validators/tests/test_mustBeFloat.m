%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('mustBeFloat'), -1);
assert_isequal(nargout('mustBeFloat'), 0);
%=============================================================================
mustBeFloat([]);
mustBeFloat(single(3));
%=============================================================================
assert_checkerror('mustBeFloat(int32(1))', _('Value must be a float.'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10),  _('Value must be a float.')];
assert_checkerror('mustBeFloat(int32(1), 3)', msg);
%=============================================================================

