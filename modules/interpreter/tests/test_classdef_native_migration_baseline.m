%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Safety net for the classdef native front-end migration.
%
% It pins the CURRENT observable behaviour of the classdef loader so that any
% accidental change during the migration is caught. Cases split in two:
%   - CONFORMING: Nelson already matches expected behavior; these must stay green.
%   - DIVERGENCE: Nelson differs from the reference; the CURRENT behaviour is
%     asserted here with the target and the phase that will fix it. When that
%     phase lands, the assertion below flips (the test breaks on purpose) and
%     must be updated to the target behaviour.
%
% Fixtures live in tests/classdef/ (prefix ClassdefMig).
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% --- CONFORMING: contextual block keywords (Q1) -----------------------------
% properties/methods/events are ordinary identifiers inside a method body.
o = ClassdefMigKeywordVars();
asserts.isequal(o.useKeywordsAsVars(), 86);
% properties(obj) still resolves to the reflection function.
names = o.callProperties();
asserts.istrue(iscellstr(names) || ischar(names) || isstring(names));
%=============================================================================
% --- CONFORMING: block comment between header and first block (Q2) ----------
o = ClassdefMigHeaderComment();
asserts.isequal(o.a, 7);
%=============================================================================
% --- CONFORMING: static method call in a property default (Q5a) -------------
o = ClassdefMigStaticDefault();
asserts.isequal(o.s, 99);
%=============================================================================
% --- CONFORMING: transpose followed by a comment in a default (F1) ----------
o = ClassdefMigTranspose();
asserts.isequal(o.v, [1; 2; 3]);
%=============================================================================
% --- CONFORMING (Q3): double-quoted access attribute value ------------------
% Reference rejects a double-quoted access value; Nelson now rejects it too
% (single-quoted and bare values still accepted). Fixed 2026-08-11.
asserts.istrue(mig_did_throw('ClassdefMigDoubleQuote()'));
%=============================================================================
% --- CONFORMING (F3): block comment INSIDE a properties block ---------------
% The scanner now blanks %{ %} block comments before scanning, so a class with
% block comments inside a block loads like the reference. Fixed 2026-08-11.
o = ClassdefMigBlockComment();
asserts.isequal(o.a, 1);
%=============================================================================
% --- CONFORMING (F2): double-quoted string default containing '%' -----------
% stripComment now tracks double quotes, so the '%' inside a double-quoted
% default no longer truncates it and the class loads. Fixed 2026-08-11.
o = ClassdefMigStrDefault();
asserts.isequal(char(o.s), '100% sure');
%=============================================================================
% --- CONFORMING (Q9a): non-literal attribute value expression ---------------
% Reference evaluates the expression (logical(1) is true) and accepts it;
% Nelson now accepts expression-shaped attribute values too. The boolean value
% is still derived syntactically (full evaluation deferred). Fixed 2026-08-11.
o = ClassdefMigAttrExpr();
asserts.isequal(o.f(), 1);
%=============================================================================
% --- CONFORMING (immutable): SetAccess = immutable --------------------------
% SetAccess = immutable is now supported: settable during construction,
% read-only afterwards. Full semantics in test_classdef_immutable. The class
% loads and constructs. Fixed 2026-08-11.
o = ClassdefMigImmutable(1, 2);
asserts.isequal(o.x, 1);
%=============================================================================
% --- DIVERGENCE (Q8): enumeration members with constructor arguments --------
% Target (reference): M -> ctor(1,2), N -> ctor(3,4) via the base ctor.
% Current (Nelson): enum arguments are not mapped, members lack the properties.
% Fix in P3.
asserts.istrue(mig_did_throw('ClassdefMigEnumArgs.M.x'));
%=============================================================================
% --- CONFORMING (Q4): misplaced x@Super in a non-constructor method ---------
% The reference rejects the whole class at load time (the left operand of '@'
% must name the enclosing method). Nelson now rejects it at load too. The
% grammar-level native @Super parse is still deferred (win_bison unavailable in
% this environment); the load-time validation delivers the observable behaviour.
% Fixed 2026-08-11.
asserts.isfalse(mig_class_loads('ClassdefMigBadSuper'));
%=============================================================================
% --- CONFORMING (Q4 companion): anonymous function in a method --------------
% An anonymous function '@(...)' assigned in a non-constructor method must not
% be misread as a misplaced x@Super(...) superclass call: the token between '@'
% and '(' is empty, which is not a qualified identifier. The class must load,
% and the method must run. Regression for a load-time validator that accepted
% the empty string as a superclass name.
asserts.istrue(mig_class_loads('ClassdefMigAnonMethod'));
o = ClassdefMigAnonMethod();
asserts.isequal(o.pick({'alpha', 'beta', 'gamma'}, 'beta'), 2);
%=============================================================================
% --- CONFORMING (Q5b): cross-property reference in a default ----------------
% The default-evaluation error is now wrapped "Invalid default value for
% property 'y' in class '...': ..." like the reference (the underlying message
% text still differs, but the wrapping matches). Fixed 2026-08-11.
asserts.throws('ClassdefMigCrossProp();', 'Invalid default value for property');
%=============================================================================
rmpath(path_test);
%=============================================================================
function tf = mig_did_throw(code)
  tf = false;
  try
    eval(code);
  catch
    tf = true;
  end
end
%=============================================================================
function tf = mig_class_loads(name)
  % True if the class definition loads without error (constructor callable),
  % independent of whether a later method call would fail.
  tf = false;
  try
    feval(name);
    tf = true;
  catch
    tf = false;
  end
end
%=============================================================================
