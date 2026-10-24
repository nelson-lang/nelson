%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function nhood = ip_nhood(se)
  if isstruct(se) && isfield(se, 'nhood')
    raw = se.nhood;
  else
    raw = se;
  end
  if ~(isnumeric(raw) || islogical(raw)) || issparse(raw) || length(size(raw)) > 3 || isempty(raw)
    error('Nelson:image_processing:structuringElementNeighborhoodNonemptyDD', 'Structuring element neighborhood must be a nonempty 2-D or 3-D numeric or logical array.');
  end
  nhood = ip_to_logical_mask(raw);
  if ~any(nhood(:))
    error('Nelson:image_processing:structuringElementNeighborhoodContainLeastOne', 'Structuring element neighborhood must contain at least one true value.');
  end
end
%=============================================================================
