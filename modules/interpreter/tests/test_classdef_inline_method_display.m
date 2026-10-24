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
% Regression: a one-line classdef method whose last body statement is
% suppressed with ';' (e.g. "function r = m(); r = 'x'; end") must NOT print
% its result when the method is used as an assignment RHS or a subexpression.
% Expanding the one-line method to its multi-line form used to drop the
% trailing ';', turning the suppressed assignment into a displayed one.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
% Semicolon-suppressed static method: no display, correct value.
semicolonAssign = evalc('valueSemicolon = ClassdefInlineMethod.staticSemicolon();');
asserts.isequal(semicolonAssign, '');
asserts.isequal(valueSemicolon, 'static-semicolon');
%=============================================================================
% Used as a subexpression (matrix element / function argument): no display.
semicolonSubexpr = evalc('joined = [''prefix: '', ClassdefInlineMethod.staticSemicolon()];');
asserts.isequal(semicolonSubexpr, '');
asserts.isequal(joined, 'prefix: static-semicolon');
%=============================================================================
% Multiple statements on one line, all suppressed: no display.
multiAssign = evalc('valueMulti = ClassdefInlineMethod.staticMulti();');
asserts.isequal(multiAssign, '');
asserts.isequal(valueMulti, 3);
%=============================================================================
% Nested end on one line: no display, inner block preserved.
nestedAssign = evalc('valueNested = ClassdefInlineMethod.staticNested();');
asserts.isequal(nestedAssign, '');
asserts.isequal(valueNested, 'static-nested');
%=============================================================================
% Instance one-line method with semicolon: no display.
inlineObject = ClassdefInlineMethod();
instanceAssign = evalc('valueInstance = inlineObject.instanceSemicolon();');
asserts.isequal(instanceAssign, '');
asserts.isequal(valueInstance, 'instance-semicolon');
%=============================================================================
% Comma terminator keeps display semantics: the result IS shown.
commaAssign = evalc('valueComma = ClassdefInlineMethod.staticComma();');
asserts.istrue(contains(commaAssign, 'r ='));
asserts.istrue(contains(commaAssign, 'static-comma'));
asserts.isequal(valueComma, 'static-comma');
%=============================================================================
rmpath(path_test);
%=============================================================================
