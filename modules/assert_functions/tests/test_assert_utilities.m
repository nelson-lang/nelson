%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('asserts.size'), 2);
asserts.isequal(nargout('asserts.size'), 2);
asserts.isequal(nargin('asserts.class'), 2);
asserts.isequal(nargout('asserts.class'), 2);
asserts.isequal(nargin('asserts.empty'), 1);
asserts.isequal(nargout('asserts.empty'), 2);
asserts.isequal(nargin('asserts.notempty'), 1);
asserts.isequal(nargout('asserts.notempty'), 2);
asserts.isequal(nargin('asserts.alltrue'), 1);
asserts.isequal(nargout('asserts.alltrue'), 2);
asserts.isequal(nargin('asserts.allfalse'), 1);
asserts.isequal(nargout('asserts.allfalse'), 2);
%=============================================================================
asserts.size(ones(2, 3), [2 3]);
[r, msg] = asserts.size(ones(2, 3), [3 2]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected size'));
asserts.istrue(contains(msg, 'computed size'));
%=============================================================================
asserts.class(1, 'double');
[r, msg] = asserts.class(single(1), 'double');
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected class'));
asserts.istrue(contains(msg, 'computed class'));
%=============================================================================
asserts.empty([]);
[r, msg] = asserts.empty(1);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected empty value'));
%=============================================================================
asserts.notempty(1);
[r, msg] = asserts.notempty([]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected non-empty value'));
%=============================================================================
asserts.alltrue([true true]);
[r, msg] = asserts.alltrue([true false]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected all entries to be true'));
%=============================================================================
asserts.allfalse([false false]);
[r, msg] = asserts.allfalse([false true]);
asserts.isfalse(r);
asserts.istrue(contains(msg, 'expected all entries to be false'));
%=============================================================================
asserts.checkerror('feval(''asserts.size'', ones(2, 3), ''2 3'')', _('Expected size must be a numeric vector.'));
asserts.checkerror('feval(''asserts.alltrue'', [1 1])', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'logical'));
%=============================================================================
