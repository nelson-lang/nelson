%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
[builtinFunctions, macroFunctions] = what();
publicFunctions = [string(builtinFunctions); string(macroFunctions)];
publicFunctions = unique(publicFunctions);
internalFunctions = startsWith(publicFunctions, "__") & endsWith(publicFunctions, "__");
publicFunctions(internalFunctions) = [];
asserts.istrue(length(publicFunctions) > 0);
%=============================================================================
for k = 1:length(publicFunctions)
  name = char(publicFunctions(k));

  allLocations = which(name, '-all');
  asserts.istrue(iscell(allLocations), ['which(''', name, ''', ''-all'') must return a cell.']);
  asserts.istrue(~isempty(allLocations), ['Missing locations for public function: ', name]);
end
%=============================================================================
