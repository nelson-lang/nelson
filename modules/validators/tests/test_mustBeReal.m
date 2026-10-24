%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeReal'), -1);
asserts.isequal(nargout('mustBeReal'), 0);
%=============================================================================
mustBeNonZero(1);
asserts.checkerror('mustBeReal(i)', sprintf(_('%s must be real.'), 'Value'), 'Nelson:validators:mustBeReal');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be real.'), 'Value')];
asserts.checkerror('mustBeReal(i, 3)', msg, 'Nelson:validators:mustBeReal');
%=============================================================================
