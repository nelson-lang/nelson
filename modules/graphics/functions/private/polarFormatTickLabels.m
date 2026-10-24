%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function labels = polarFormatTickLabels(values, appendDegree)
  labels = cell(numel(values), 1);
  for k = 1:numel(values)
    value = values(k);
    if abs(value - round(value)) < 1e-12
      label = int2str(round(value));
    else
      label = num2str(value, 12);
    end
    if appendDegree
      label = [label, char(176)];
    end
    labels{k} = label;
  end
end
%=============================================================================
