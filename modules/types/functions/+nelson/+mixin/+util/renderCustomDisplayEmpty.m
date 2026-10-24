%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Default CustomDisplay rendering for an empty object array: dimensions, class
% name and the list of property names. In .m so the formatting can evolve
% without rebuilding the interpreter.
%=============================================================================
function renderCustomDisplayEmpty(obj)
  dims = sprintf('%dx', size(obj));
  dims(end) = [];
  names = properties(obj);
  if isempty(names)
    disp(['  ', dims, ' ', class(obj), ' array with no properties.']);
  else
    disp(['  ', dims, ' ', class(obj), ' array with properties:']);
    fprintf('\n');
    for k = 1:numel(names)
      disp(['    ', names{k}]);
    end
  end
  fprintf('\n');
end
%=============================================================================
