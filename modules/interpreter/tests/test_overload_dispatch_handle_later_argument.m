%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A user handle class object in a later argument selects its method; native
% handles and the accessors generated for every handle class do not.
%=============================================================================
path_test = [tempdir(), createGUID()];
mkdir(path_test);
filewrite([path_test, '/OverloadDispatchLaterHandle.m'], ...
  ["classdef OverloadDispatchLaterHandle < handle"; ...
  "  properties"; ...
  "    Value = 1"; ...
  "  end"; ...
  "  methods"; ...
  "    function r = overloadDispatchLaterFoo(varargin)"; ...
  "      r = 'handle method';"; ...
  "    end"; ...
  "  end"; ...
  "end"]);
filewrite([path_test, '/overloadDispatchLaterFoo.m'], ...
  ["function r = overloadDispatchLaterFoo(varargin)"; ...
  "  r = 'plain function';"; ...
  "end"]);
addpath(path_test);
%=============================================================================
h = OverloadDispatchLaterHandle();
asserts.isequal(overloadDispatchLaterFoo(h), 'handle method');
asserts.isequal(overloadDispatchLaterFoo(5, h), 'handle method');
asserts.isequal(overloadDispatchLaterFoo('a', 2, h), 'handle method');
asserts.isequal(overloadDispatchLaterFoo(5, 6), 'plain function');
%=============================================================================
% native handles in a later position keep the plain function
m = containers.Map();
asserts.isequal(overloadDispatchLaterFoo(5, m), 'plain function');
%=============================================================================
% generated handle accessors are not reached from a later position
s = struct('a', h);
asserts.istrue(isstruct(s));
asserts.istrue(isa(s.a, 'OverloadDispatchLaterHandle'));
asserts.isequal(fieldnames(struct('b', h)), {'b'});
%=============================================================================
rmpath(path_test);
rmdir(path_test, 's');
%=============================================================================
