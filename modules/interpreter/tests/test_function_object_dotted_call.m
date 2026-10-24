%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% fn.Member.Method(args): a dotted call whose head is a zero-argument function
% returning an object must evaluate the head and apply the member chain, even
% when a segment starts with an uppercase letter (the lexical hint for a class
% name). Only when the dotted name is neither a classdef static/constructor nor
% a package function does the chain fallback apply.
%=============================================================================
subdir = [modulepath('interpreter', 'tests'), '/classdef/fn_object_chain'];
addpath(subdir);
%=============================================================================
% uppercase middle segment and uppercase leaf (script top level)
assert_isequal(chainroot.Inner.Scale(3), 30);
assert_isequal(chainroot.inner.Scale(4), 40);
assert_isequal(chainroot.Inner.add(5), 6);
assert_isequal(chainroot.inner.add(6), 7);
%=============================================================================
% with a trailing index on the result
r = chainroot.Inner.Scale([1 2 3]);
assert_isequal(r, [10 20 30]);
%=============================================================================
% inside a compiled function body
[a, b, c] = chain_in_function();
assert_isequal(a, 30);
assert_isequal(b, 40);
assert_isequal(c, 6);
%=============================================================================
% immediate evaluation (same chunk as addpath)
cmd = ['rmpath(''', subdir, '''); addpath(''', subdir, '''); v = chainroot.Inner.Scale(8);'];
eval(cmd);
assert_isequal(v, 80);
%=============================================================================
% an unknown leaf is reported by the object, not as an undefined function
assert_checkerror('chainroot.Inner.Missing(1)', sprintf(_('No such property: %s'), 'Missing'));
%=============================================================================
% a variable with the same name as the head keeps variable semantics
chainroot = struct('Inner', struct('Scale', [5 6 7]));
assert_isequal(chainroot.Inner.Scale(2), 6);
clear chainroot
%=============================================================================
rmpath(subdir);
%=============================================================================
