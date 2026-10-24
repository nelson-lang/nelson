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
% A nelson.mixin.CustomCompactDisplayProvider object provides its own compact
% representation when shown inside a container (cell/struct), via
% compactRepresentationForSingleLine, which returns a
% nelson.display.CompactDisplayRepresentation built with the inherited
% widthConstrainedDataRepresentation helper.
%=============================================================================
d = [tempname(), '/'];
mkdir(d);
%=============================================================================
filewrite([d, 'Temp.m'], [...
  "classdef Temp < nelson.mixin.CustomCompactDisplayProvider"; ...
  "  properties"; ...
  "    Celsius = 0"; ...
  "  end"; ...
  "  methods"; ...
  "    function obj = Temp(c)"; ...
  "      if nargin > 0"; ...
  "        obj.Celsius = c;"; ...
  "      end"; ...
  "    end"; ...
  "    function rep = compactRepresentationForSingleLine(obj, config, width)"; ...
  "      txt = [num2str(obj.Celsius), ' degC'];"; ...
  "      rep = widthConstrainedDataRepresentation(obj, config, width, 'StringArray', string(txt));"; ...
  "    end"; ...
  "  end"; ...
  "end"]);
addpath(d);
%=============================================================================
t = Temp(20);
assert_istrue(isa(t, 'nelson.mixin.CustomCompactDisplayProvider'));
%=============================================================================
% the method returns a nelson.display.CompactDisplayRepresentation
cfg = nelson.display.DisplayConfiguration();
rep = t.compactRepresentationForSingleLine(cfg, 80);
assert_istrue(isa(rep, 'nelson.display.CompactDisplayRepresentation'));
assert_isequal(char(rep.PaddedDisplayOutput), '20 degC');
assert_isequal(rep.CharacterWidth, 7);
%=============================================================================
% width constraint truncates with the configured Ellipsis
repw = t.compactRepresentationForSingleLine(cfg, 4);
assert_istrue(numel(char(repw.PaddedDisplayOutput)) <= 4);
%=============================================================================
% inside a cell, the object shows its compact representation, not [1x1 Temp]
c = {Temp(20), Temp(37)};
s = evalc('disp(c)');
assert_istrue(contains(s, '20 degC'));
assert_istrue(contains(s, '37 degC'));
assert_isfalse(contains(s, '[1x1 Temp]'));
%=============================================================================
% inside a table column, the object shows its compact representation
T = table([Temp(20); Temp(37)], 'VariableNames', {'Temperature'});
s = evalc('disp(T)');
assert_istrue(contains(s, '20 degC'));
assert_istrue(contains(s, '37 degC'));
%=============================================================================
rmpath(d);
%=============================================================================
