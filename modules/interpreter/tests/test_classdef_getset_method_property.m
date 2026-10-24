%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A method named 'get'/'set' must not intercept dot-access to a property that
% lives in an attributed 'properties (...)' block.
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = ClassdefGetSetMethodProperty();
%=============================================================================
% a 'get' method does not intercept property reads
assert_isequal(o.counter, 42);
assert_isequal(o.readCounter(), 42);
%=============================================================================
% a 'set' method does not intercept property writes
o.counter = 100;
assert_isequal(o.counter, 100);
assert_isequal(o.readCounter(), 100);
%=============================================================================
% private property is read/written from within the class
o.bumpSecret();
assert_isequal(o.readSecret(), 8);
%=============================================================================
% the explicit get/set methods remain callable
assert_isequal(o.get('anything'), 999);
%=============================================================================
% private property remains inaccessible from outside the class
failed = false;
try
  value = o.secret; %#ok<NASGU>
catch
  failed = true;
end
assert_istrue(failed);
%=============================================================================
