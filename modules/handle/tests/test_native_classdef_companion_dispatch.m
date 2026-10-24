%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Builtins called on native companion handles (listener, dynamic property
% descriptor, weak reference storage) do not probe a class file for them.
%=============================================================================
testdir = [tempname(), '/'];
mkdir(testdir);
filewrite([testdir, '/NelsonCompanionSource.m'], [
  "classdef NelsonCompanionSource < dynamicprops";
  "  events";
  "    Ping";
  "  end";
  "end"]);
addpath(testdir);
%=============================================================================
src = NelsonCompanionSource();
l = addlistener(src, 'Ping', @(varargin) []);
asserts.isequal(class(l), 'event.listener');
asserts.isfalse(isempty(l));
asserts.isequal(numel(l), 1);
asserts.isequal(size(l), [1 1]);
asserts.istrue(isa(l, 'event.listener'));
asserts.istrue(isa(l, 'handle'));
asserts.istrue(isvalid(l));
delete(l);
asserts.isfalse(isvalid(l));
asserts.isequal(class(l), 'event.listener');
%=============================================================================
p = addprop(src, 'Extra');
asserts.isequal(class(p), 'meta.DynamicProperty');
asserts.isfalse(isempty(p));
asserts.isequal(numel(p), 1);
asserts.istrue(isa(p, 'meta.DynamicProperty'));
asserts.istrue(isvalid(p));
asserts.isequal(p.Name, 'Extra');
src.Extra = 3;
asserts.isequal(src.Extra, 3);
delete(p);
asserts.isfalse(isvalid(p));
asserts.isfalse(isprop(src, 'Extra'));
%=============================================================================
s = __classdef_weak_reference_new__();
asserts.isequal(class(s), '__classdef_weak_reference_storage__');
asserts.isfalse(contains(class(s), '.'));
asserts.isfalse(isempty(s));
asserts.isequal(numel(s), 1);
asserts.istrue(isa(s, 'handle'));
target = NelsonCompanionSource();
__classdef_weak_reference_set__(s, target);
asserts.istrue(__classdef_weak_reference_get__(s) == target);
delete(target);
asserts.isfalse(isvalid(__classdef_weak_reference_get__(s)));
%=============================================================================
rmpath(testdir);
%=============================================================================
