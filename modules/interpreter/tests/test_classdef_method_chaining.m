%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Chained method/dispatch on an expression result: a method or function that
% returns an object can be immediately subscripted again, so a fluent API
% (obj.m(a).m(b), f().m(), obj.m().field) works as a statement, not only when
% the intermediate value is stored in a variable first.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/classdef/']);
%=============================================================================
% handle: method().method() as a quiet statement (the intermediate object must
% survive so the second method runs on the same handle).
c = ClassdefFluentHandle();
c.tag('a').tag('b');
asserts.isequal(c.Trail, {'a', 'b'});
%=============================================================================
% handle: three chained methods accumulate on the same handle.
c = ClassdefFluentHandle();
c.add(1).add(2).add(3);
asserts.isequal(c.total(), 6);
%=============================================================================
% handle: method().property reads a property of the method result.
c = ClassdefFluentHandle();
v = c.add(10).Value;
asserts.isequal(v, 10);
%=============================================================================
% handle: mixed obj.m().field used as a value.
c = ClassdefFluentHandle();
c.tag('x');
t = c.tag('y').Trail;
asserts.isequal(t, {'x', 'y'});
%=============================================================================
% handle: method().method().property combines call chaining and a field read.
c = ClassdefFluentHandle();
w = c.add(1).add(2).Value;
asserts.isequal(w, 3);
%=============================================================================
% function_returning_object().method(): the call result is chained directly.
h = makeFluentHandle(4).add(6);
asserts.isequal(h.total(), 10);
%=============================================================================
% function_returning_object().method().method() as a quiet statement.
h = makeFluentHandle(0);
h.add(2).add(5);
asserts.isequal(h.total(), 7);
%=============================================================================
% constructor_result().method() (a constructor is a function returning an object).
c = ClassdefFluentHandle(1).add(2).add(3);
asserts.isequal(c.total(), 6);
%=============================================================================
% printed form (no semicolon) keeps working and sets ans.
c = ClassdefFluentHandle(100);
printed = evalc('c.add(1).add(1)');
asserts.istrue(contains(printed, 'ans ='));
asserts.isequal(c.total(), 102);
%=============================================================================
% regression: a single method call still works.
c = ClassdefFluentHandle();
c.tag('only');
asserts.isequal(c.Trail, {'only'});
%=============================================================================
% regression: storing the intermediate still works.
c = ClassdefFluentHandle();
t = c.tag('p');
t.tag('q');
asserts.isequal(c.Trail, {'p', 'q'});
%=============================================================================
% value class: chaining on the temporary copies and reading the final result.
v = ClassdefFluentValue();
r = v.add(2).add(3).total();
asserts.isequal(r, 5);
% value semantics: the original is untouched by the chain.
asserts.isequal(v.total(), 0);
%=============================================================================
% value class: method().property on a copy.
r2 = ClassdefFluentValue(7).add(3).Value;
asserts.isequal(r2, 10);
%=============================================================================
% value class: a quiet chain runs without error and leaves the source untouched.
w = ClassdefFluentValue(10);
w.add(1).add(2);
asserts.isequal(w.total(), 10);
%=============================================================================
