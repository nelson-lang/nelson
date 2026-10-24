%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNonZero'), -1);
asserts.isequal(nargout('mustBeNonZero'), 0);
%=============================================================================
mustBeNonZero(1);
asserts.checkerror('mustBeNonZero(0)', sprintf(_('%s must be nonzero.'), 'Value'), 'Nelson:validators:mustBeNonZero');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be nonzero.'), 'Value')];
asserts.checkerror('mustBeNonZero(0, 3)', msg, 'Nelson:validators:mustBeNonZero');
%=============================================================================
% the reference spelling is lower case (mustBeNonzero); both are accepted
mustBeNonzero(1);
mustBeNonzero([1 2 3]);
asserts.checkerror('mustBeNonzero(0)', sprintf(_('%s must be nonzero.'), 'Value'), 'Nelson:validators:mustBeNonZero');
%=============================================================================
