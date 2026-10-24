%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeNonSparse'), -1);
asserts.isequal(nargout('mustBeNonSparse'), 0);
%=============================================================================
mustBeNonSparse(1);
asserts.checkerror('mustBeNonSparse(sparse(0))', sprintf(_('%s must be nonsparse.'), 'Value'), 'Nelson:validators:mustBeNonSparse');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be nonsparse.'), 'Value')];
asserts.checkerror('mustBeNonSparse(sparse(0), 3)', msg, 'Nelson:validators:mustBeNonSparse');
%=============================================================================
% the reference spelling is lower case (mustBeNonsparse); both are accepted
mustBeNonsparse(1);
mustBeNonsparse([1 2 3]);
%=============================================================================
