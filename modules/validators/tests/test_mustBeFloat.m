%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeFloat'), -1);
asserts.isequal(nargout('mustBeFloat'), 0);
%=============================================================================
mustBeFloat([]);
mustBeFloat(single(3));
%=============================================================================
asserts.checkerror('mustBeFloat(int32(1))', sprintf(_('%s must be a float.'), 'Value'));
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a float.'), 'Value')];
asserts.checkerror('mustBeFloat(int32(1), 3)', msg);
%=============================================================================

