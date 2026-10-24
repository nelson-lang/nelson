%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function s = __tableStructFromCell__(values, names)
  % Build a table backing data struct from a cell of values and a cell of
  % variable names. Unlike cell2struct, this accepts table variable names that
  % are not valid identifiers (for example names containing spaces), which the
  % table interface allows. Appending the fields one at a time is linear
  % because adding a struct field is amortized O(1).
  s = struct();
  for k = 1:numel(names)
    s.(names{k}) = values{k};
  end
end
%=============================================================================
