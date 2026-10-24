%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('regexprep'), -3);
asserts.isequal(nargout('regexprep'), 1);
%=============================================================================
r = regexprep('bat cat coat', 'c[aeiou]+t', 'dog');
asserts.isequal(r, 'bat dog dog');
%=============================================================================
r = regexprep('a1 b2', '(\w)(\d)', '$2-$1');
asserts.isequal(r, '1-a 2-b');
%=============================================================================
r = regexprep('abc123', '(?<letters>[a-z]+)(?<digits>\d+)', '$<digits>:$<letters>');
asserts.isequal(r, '123:abc');
%=============================================================================
r = regexprep('a1 b2 c3', '(\w)(\d)', '$2-$1', 'once');
asserts.isequal(r, '1-a b2 c3');
%=============================================================================
r = regexprep('a1 b2 c3', '(\w)(\d)', '$2-$1', 2);
asserts.isequal(r, 'a1 2-b c3');
%=============================================================================
r = regexprep('My flowers may bloom in May', 'M(\w+)y', 'April', 'preservecase');
asserts.isequal(r, 'My flowers april bloom in April');
%=============================================================================
r = regexprep({'a1'; 'b2'}, '(\w)(\d)', '$2-$1');
asserts.isequal(r, {'1-a'; '2-b'});
%=============================================================================
r = regexprep("a1", '(\w)(\d)', '$2-$1');
asserts.isequal(r, "1-a");
%=============================================================================
r = regexprep('a1', '(\w)(\d)', '${upper($1)}');
asserts.isequal(r, 'A');
%=============================================================================
% Unbalanced metacharacters parse as literals, not errors.
asserts.isequal(regexprep('a(b', '(', 'X'), 'aXb');
asserts.isequal(regexprep('a)b', ')', 'X'), 'aXb');
asserts.isequal(regexprep('a[b', '[', 'X'), 'aXb');
% escaped metacharacter behaves identically (exact, no leniency)
asserts.isequal(regexprep('a(b', '\(', 'X'), 'aXb');
%=============================================================================
