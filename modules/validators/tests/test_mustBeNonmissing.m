%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNonmissing'), -1);
asserts.isequal(nargout('mustBeNonmissing'), 0);
%=============================================================================
mustBeNonmissing([]);
mustBeNonmissing([true, false]);
asserts.checkerror('mustBeNonmissing(["hello" string(NaN)])', sprintf(_('%s must be nonmissing.'), 'Value'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be nonmissing.'), 'Value')];
asserts.checkerror('mustBeNonmissing(NaN, 3)', msg);
%=============================================================================
