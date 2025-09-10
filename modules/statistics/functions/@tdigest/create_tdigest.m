%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function td = create_tdigest(compression, totalWeight, means, weights)
  td.compression = compression;
  td.totalWeight = totalWeight;
  td.means = means;
  td.weights = weights;
  td.version = 1.0;
  
  if (compression <= 0)
    error(_("Compression must be a positive scalar."));
  end
  td = class(td, 'tdigest');
end
%=============================================================================
