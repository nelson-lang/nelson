%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeVectorOrEmpty'), -1);
asserts.isequal(nargout('mustBeVectorOrEmpty'), 0);
%=============================================================================
mustBeVectorOrEmpty(3);
mustBeVectorOrEmpty([1, 2]);
mustBeVectorOrEmpty([1; 2]);
mustBeVectorOrEmpty([]);
mustBeVectorOrEmpty(zeros(0, 3));
%=============================================================================
msg = sprintf(_('%s must be a 1-by-n vector, an n-by-1 vector, or empty.'), 'Value');
asserts.checkerror('mustBeVectorOrEmpty(ones(2))', msg, 'Nelson:validators:mustBeVectorOrEmpty');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a 1-by-n vector, an n-by-1 vector, or empty.'), 'Value')];
asserts.checkerror('mustBeVectorOrEmpty(ones(2), 3)', msg, 'Nelson:validators:mustBeVectorOrEmpty');
%=============================================================================
% empty arrays of any dimension and any type implementing isvector and isempty
mustBeVectorOrEmpty(zeros(0, 3, 2));
mustBeVectorOrEmpty(ones(2, 2, 0));
mustBeVectorOrEmpty([1i, 2]);
mustBeVectorOrEmpty('abc');
mustBeVectorOrEmpty(["a"; "b"]);
mustBeVectorOrEmpty({1, 2});
mustBeVectorOrEmpty(struct('a', {1, 2}));
mustBeVectorOrEmpty({});
msg = sprintf(_('%s must be a 1-by-n vector, an n-by-1 vector, or empty.'), 'Value');
asserts.checkerror('mustBeVectorOrEmpty(ones(1, 1, 3))', msg, 'Nelson:validators:mustBeVectorOrEmpty');
asserts.checkerror('mustBeVectorOrEmpty(cell(2, 2))', msg, 'Nelson:validators:mustBeVectorOrEmpty');
asserts.checkerror('mustBeVectorOrEmpty(["a", "b"; "c", "d"])', msg, 'Nelson:validators:mustBeVectorOrEmpty');
%=============================================================================
