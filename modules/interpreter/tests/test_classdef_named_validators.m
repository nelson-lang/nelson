%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
% property validators referencing the property name are called as written
o = ClassdefNamedValidators();
asserts.isequal(o.Mode, 'auto');
asserts.isequal(o.Level, 5);
asserts.isequal(o.Count, 1);
asserts.isequal(o.Word, "abc");
%=============================================================================
o.Mode = 'manual';
asserts.isequal(o.Mode, 'manual');
asserts.checkerror('o.Mode = ''other'';', ...
  _('Value must be member of the compared value.'), 'Nelson:validators:mustBeMember');
%=============================================================================
o.Level = 10;
asserts.isequal(o.Level, 10);
asserts.checkerror('o.Level = 11;', ...
  _('Value must be in range.'), 'Nelson:validators:LeftClosedRightClosed');
%=============================================================================
% mix of implicit-first-arg validator and named validator on the same property
o.Count = 7;
asserts.isequal(o.Count, 7);
asserts.checkerror('o.Count = 0;', ...
  sprintf(_('Value must exceed %s.'), '0'), 'Nelson:validators:mustBeGreaterThan');
asserts.checkerror('o.Count = 1.5;', ...
  _('Value must be an integer.'), 'Nelson:validators:mustBeInteger');
%=============================================================================
o.Word = "def";
asserts.isequal(o.Word, "def");
asserts.checkerror('o.Word = "xyz";', ...
  _('Value must be member of the compared value.'), 'Nelson:validators:mustBeMember');
%=============================================================================
