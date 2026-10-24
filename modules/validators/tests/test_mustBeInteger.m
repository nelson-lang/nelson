%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeInteger'), -1);
asserts.isequal(nargout('mustBeInteger'), 0);
%=============================================================================
mustBeInteger([])
mustBeInteger([1, 2]);
asserts.checkerror('mustBeInteger(NaN)', sprintf(_('%s must be an integer.'), 'Value'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be an integer.'), 'Value')];
asserts.checkerror('mustBeInteger(1/3, 3)', msg);
%=============================================================================


