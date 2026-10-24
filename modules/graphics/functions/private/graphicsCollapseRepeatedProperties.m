%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function propertiesList = graphicsCollapseRepeatedProperties(propertiesList)
  % Keeps only the last value of a property named more than once, as every
  % graphics constructor does. Use it wherever a property list is turned into a
  % struct: a struct refuses a duplicated field name, and a list built by
  % appending caller properties to a set of defaults holds repeats by design.
  nbProperties = length(propertiesList);
  if (nbProperties < 4)
    return
  end
  keep = true(1, nbProperties);
  for k = 1:2:nbProperties - 1
    if ~ischar(propertiesList{k})
      continue
    end
    for j = k + 2:2:nbProperties - 1
      if ischar(propertiesList{j}) && strcmpi(propertiesList{k}, propertiesList{j})
        keep(k:k + 1) = false;
        break
      end
    end
  end
  propertiesList = propertiesList(keep);
end
%=============================================================================
