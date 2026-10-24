%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
[builtinFunctions, macroFunctions] = what();
publicFunctions = [string(builtinFunctions); string(macroFunctions)];
publicFunctions = unique(publicFunctions);
internalFunctions = startsWith(publicFunctions, "__") & endsWith(publicFunctions, "__");
publicFunctions(internalFunctions) = [];
asserts.istrue(length(publicFunctions) > 0);
%=============================================================================
for k = 1:length(publicFunctions)
  name = char(publicFunctions(k));

  modules = which(name, '-module');
  asserts.istrue(iscell(modules), ['which(''', name, ''', ''-module'') must return a cell.']);
  asserts.istrue(~isempty(modules), ['Missing module for public function: ', name]);

  location = which(name);
  asserts.istrue(ischar(location), ['which(''', name, ''') must return a character row vector.']);
  asserts.istrue(~isempty(location), ['Missing location for public function: ', name]);
end
%=============================================================================
