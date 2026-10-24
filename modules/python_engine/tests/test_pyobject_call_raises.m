%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
% An exception raised inside a Python callable must propagate unchanged. It
% must NOT be masked by the name/value keyword retry, which used to reinterpret
% a leading string positional argument as a keyword name and report a bogus
% "unexpected keyword argument" TypeError.
%=============================================================================
code = ["def f(x, y):"; ...
        "    if x == 'boom':"; ...
        "        raise ValueError('the real error')"; ...
        "    return x"; ...
        "mod = __import__('types').SimpleNamespace(f=f)"];
m = pyrun(char(join(code, newline())), "mod");
%=============================================================================
% two positional string arguments, function returns normally
r = m.f('fine', 'zzz');
assert_isequal(char(r), 'fine');
%=============================================================================
% the function body raises: the real ValueError must surface
msg = '';
try
  m.f('boom', 'zzz');
catch e
  msg = e.message;
end
assert_istrue(~isempty(msg));
assert_istrue(contains(msg, 'the real error'));
assert_isfalse(contains(msg, 'unexpected keyword argument'));
%=============================================================================
% a genuine name/value call keeps working
code2 = ["def g(a, name=None):"; ...
         "    return name"; ...
         "mod = __import__('types').SimpleNamespace(g=g)"];
m2 = pyrun(char(join(code2, newline())), "mod");
r = m2.g(1, 'name', 'hello');
assert_isequal(char(r), 'hello');
%=============================================================================
% pyargs keyword call keeps working
r = m2.g(1, pyargs('name', 'world'));
assert_isequal(char(r), 'world');
%=============================================================================
