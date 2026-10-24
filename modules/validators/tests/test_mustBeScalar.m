%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('mustBeScalar'), -1);
asserts.isequal(nargout('mustBeScalar'), 0);
%=============================================================================
mustBeScalar(1);
mustBeScalar(true);
mustBeScalar('a');
%=============================================================================
asserts.checkerror('mustBeScalar([1 2])', sprintf(_('%s must be a scalar.'), 'Value'), 'Nelson:validators:mustBeScalar');
%=============================================================================
asserts.checkerror('mustBeScalar(zeros(0, 1))', sprintf(_('%s must be a scalar.'), 'Value'), 'Nelson:validators:mustBeScalar');
%=============================================================================
msg = [sprintf(_('Invalid input argument at position %d.'), 3), char(10), sprintf(_('%s must be a scalar.'), 'Value')];
asserts.checkerror('mustBeScalar([1 2], 3)', msg);
%=============================================================================
% any type implementing isscalar
mustBeScalar(1i);
mustBeScalar(int8(3));
mustBeScalar("abc");
mustBeScalar({[1 2 3]});
mustBeScalar(struct('a', 1));
msg = sprintf(_('%s must be a scalar.'), 'Value');
asserts.checkerror('mustBeScalar([])', msg, 'Nelson:validators:mustBeScalar');
asserts.checkerror('mustBeScalar(["a", "b"])', msg, 'Nelson:validators:mustBeScalar');
asserts.checkerror('mustBeScalar({1, 2})', msg, 'Nelson:validators:mustBeScalar');
asserts.checkerror('mustBeScalar(struct(''a'', {1, 2}))', msg, 'Nelson:validators:mustBeScalar');
asserts.checkerror('mustBeScalar(''ab'')', msg, 'Nelson:validators:mustBeScalar');
%=============================================================================
