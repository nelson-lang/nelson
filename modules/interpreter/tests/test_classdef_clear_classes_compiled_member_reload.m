%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% A `ClassName.Member` access is resolved when the enclosing code is compiled.
% If the class is loaded at that point, the access is lowered to a direct call
% to the generated member function. `clear classes` unregisters that function,
% so a compiled unit that clears classes and then re-accesses the member (e.g.
% an `eval` block or a function called again after the class was reloaded) must
% reload the class instead of reporting it undefined.
%=============================================================================
reload_dir = tempname();
mkdir(reload_dir);
%=============================================================================
filewrite([reload_dir, '/CompiledMemberConst.m'], ["classdef CompiledMemberConst"; ...
  "  properties (Constant)"; ...
  "    Version = 1"; ...
  "  end"; ...
  "  methods (Static)"; ...
  "    function r = staticValue()"; ...
  "      r = 10;"; ...
  "    end"; ...
  "  end"; ...
  "end"]);
filewrite([reload_dir, '/CompiledMemberEnum.m'], ["classdef CompiledMemberEnum"; ...
  "  enumeration"; ...
  "    Red"; ...
  "    Blue"; ...
  "  end"; ...
  "end"]);
addpath(reload_dir);
%=============================================================================
% Load the classes so the accesses below are compiled to the fast direct-call
% path (the class is present in the registry at compile time).
asserts.isequal(CompiledMemberConst.Version, 1);
asserts.isequal(CompiledMemberConst.staticValue(), 10);
firstMember = CompiledMemberEnum.Red;
asserts.isequal(class(firstMember), 'CompiledMemberEnum');
%=============================================================================
% Each eval string is compiled as a single unit while the class is loaded, then
% `clear classes` runs before the member access executes. The access must
% self-heal by reloading the class.
eval('clear classes; reloadedConstant = CompiledMemberConst.Version;');
asserts.isequal(reloadedConstant, 1);
%=============================================================================
eval('clear classes; reloadedStatic = CompiledMemberConst.staticValue();');
asserts.isequal(reloadedStatic, 10);
%=============================================================================
eval('clear classes; reloadedMember = CompiledMemberEnum.Blue;');
asserts.isequal(class(reloadedMember), 'CompiledMemberEnum');
%=============================================================================
rmpath(reload_dir);
rmdir(reload_dir, 's');
%=============================================================================
