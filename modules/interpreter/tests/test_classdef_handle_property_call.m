%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Calling a function-handle stored in a property, obj.Prop(args), reads the
% property and calls the handle -- even when a method whose name differs only in
% case exists. obj.method() still dispatches to the method (case-sensitive).
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
o = ClassdefHandlePropertyCall();
%=============================================================================
% Property access alone returns the handle.
assert_isequal(class(o.Inv), 'function_handle');
%=============================================================================
% Property access with arguments calls the stored handle (not the 'inv' method).
assert_isequal(o.Inv(5), 10);
%=============================================================================
% The case-differing method is still reachable.
assert_isequal(o.inv(), 99);
%=============================================================================
