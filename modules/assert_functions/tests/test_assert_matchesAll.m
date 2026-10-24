%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.matchesAll'), 2);
asserts.isequal(nargout('asserts.matchesAll'), 2);
%=============================================================================
asserts.matchesAll('abc123', {'^abc', '[0-9]+$'});
%=============================================================================
[r, msg] = asserts.matchesAll('abc123', {'^abc', '^xyz'});
asserts.isfalse(r);
asserts.istrue(contains(msg, 'asserts.matchesAll'));
%=============================================================================
