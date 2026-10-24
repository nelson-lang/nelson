%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelson.mixin.SetGetExactNames behaves like nelson.mixin.SetGet but matches
% property names case-sensitively.
%=============================================================================
d = [tempname(), '/'];
mkdir(d);
%=============================================================================
filewrite([d, 'ExactBag.m'], [...
  "classdef ExactBag < nelson.mixin.SetGetExactNames"; ...
  "  properties"; ...
  "    Alpha = 1"; ...
  "    Beta = 2"; ...
  "  end"; ...
  "end"]);
filewrite([d, 'LooseBag.m'], [...
  "classdef LooseBag < nelson.mixin.SetGet"; ...
  "  properties"; ...
  "    Alpha = 1"; ...
  "    Beta = 2"; ...
  "  end"; ...
  "end"]);
addpath(d);
%=============================================================================
e = ExactBag();
assert_istrue(isa(e, 'nelson.mixin.SetGetExactNames'));
assert_istrue(isa(e, 'nelson.mixin.SetGet'));
%=============================================================================
% exact-case get/set works
assert_isequal(get(e, 'Alpha'), 1);
set(e, 'Beta', 20);
assert_isequal(get(e, 'Beta'), 20);
%=============================================================================
% wrong-case name is rejected (case-sensitive)
R = false;
try
  get(e, 'alpha');
catch
  R = true;
end
assert_istrue(R);
R = false;
try
  set(e, 'beta', 99);
catch
  R = true;
end
assert_istrue(R);
%=============================================================================
% the plain SetGet mixin still matches case-insensitively
l = LooseBag();
assert_isequal(get(l, 'alpha'), 1);
set(l, 'BETA', 30);
assert_isequal(get(l, 'Beta'), 30);
%=============================================================================
rmpath(d);
%=============================================================================
