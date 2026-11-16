%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fixtureDirectory = tempname();
mkdir(fixtureDirectory);
addpath(fixtureDirectory);
%=============================================================================
% Class with two value-only properties (A, B) plus a name-value-only one (C).
filewrite([fixtureDirectory, '/ClassdefSetPropsValueOnly.m'], ...
  ['classdef ClassdefSetPropsValueOnly < handle', newline(), ...
  '  properties', newline(), ...
  '    A = 0', newline(), ...
  '    B = 0', newline(), ...
  '    C = 0', newline(), ...
  '  end', newline(), ...
  '  methods', newline(), ...
  '    function obj = ClassdefSetPropsValueOnly(varargin)', newline(), ...
  '      setProperties(obj, nargin, varargin{:}, ''A'', ''B'');', newline(), ...
  '    end', newline(), ...
  '  end', newline(), ...
  'end']);
% Class without any value-only property (pure name-value form).
filewrite([fixtureDirectory, '/ClassdefSetPropsNameValue.m'], ...
  ['classdef ClassdefSetPropsNameValue < handle', newline(), ...
  '  properties', newline(), ...
  '    Value = 1', newline(), ...
  '  end', newline(), ...
  '  methods', newline(), ...
  '    function obj = ClassdefSetPropsNameValue(varargin)', newline(), ...
  '      setProperties(obj, nargin, varargin{:});', newline(), ...
  '    end', newline(), ...
  '  end', newline(), ...
  'end']);
rehash();
%=============================================================================
% Default construction: nothing is set.
obj = ClassdefSetPropsValueOnly();
asserts.isequal(obj.A, 0);
asserts.isequal(obj.B, 0);
asserts.isequal(obj.C, 0);
%=============================================================================
% Single value-only argument.
obj = ClassdefSetPropsValueOnly(10);
asserts.isequal(obj.A, 10);
asserts.isequal(obj.B, 0);
%=============================================================================
% Two value-only arguments.
obj = ClassdefSetPropsValueOnly(10, 20);
asserts.isequal(obj.A, 10);
asserts.isequal(obj.B, 20);
%=============================================================================
% Value-only arguments followed by a name-value pair.
obj = ClassdefSetPropsValueOnly(10, 20, 'C', 30);
asserts.isequal(obj.A, 10);
asserts.isequal(obj.B, 20);
asserts.isequal(obj.C, 30);
%=============================================================================
% Name-value pair only (no leading value-only argument).
obj = ClassdefSetPropsValueOnly('C', 30);
asserts.isequal(obj.A, 0);
asserts.isequal(obj.B, 0);
asserts.isequal(obj.C, 30);
%=============================================================================
% One value-only argument then a name-value pair.
obj = ClassdefSetPropsValueOnly(10, 'C', 30);
asserts.isequal(obj.A, 10);
asserts.isequal(obj.B, 0);
asserts.isequal(obj.C, 30);
%=============================================================================
% A property name wins over value-only, even when it is a value-only name.
obj = ClassdefSetPropsValueOnly('A', 5);
asserts.isequal(obj.A, 5);
asserts.isequal(obj.B, 0);
%=============================================================================
% Several name-value pairs.
obj = ClassdefSetPropsValueOnly('A', 5, 'B', 7);
asserts.isequal(obj.A, 5);
asserts.isequal(obj.B, 7);
%=============================================================================
% A leading value-only argument then a name that is a value-only name.
obj = ClassdefSetPropsValueOnly(10, 'B', 7);
asserts.isequal(obj.A, 10);
asserts.isequal(obj.B, 7);
%=============================================================================
% String (double-quoted) property name is accepted.
obj = ClassdefSetPropsValueOnly("B", 7);
asserts.isequal(obj.B, 7);
%=============================================================================
% Too many value-only arguments for the declared value-only names.
asserts.checkerror('ClassdefSetPropsValueOnly(10, 20, 30);', ...
  sprintf(_('Too many value-only inputs: at most %d expected, %d supplied. A common cause is a misspelled property name.'), 2, 3));
%=============================================================================
% Pure name-value form.
obj = ClassdefSetPropsNameValue('Value', 9);
asserts.isequal(obj.Value, 9);
obj = ClassdefSetPropsNameValue();
asserts.isequal(obj.Value, 1);
%=============================================================================
% Odd number of name-value arguments.
asserts.checkerror('ClassdefSetPropsNameValue("Value");', ...
  _('Property arguments must be name-value pairs.'));
%=============================================================================
% numArgs larger than the number of supplied property arguments.
obj = ClassdefSetPropsNameValue();
asserts.checkerror('setProperties(obj, 3, ''Value'', 6);', ...
  sprintf(_('Wrong value for argument #%d: value cannot exceed the number of supplied property arguments.'), 2));
%=============================================================================
clear classes;
rmpath(fixtureDirectory);
rmdir(fixtureDirectory, 's');
%=============================================================================
