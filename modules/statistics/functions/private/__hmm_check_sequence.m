%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function seq = __hmm_check_sequence(seq, upperBound, name)
  if ~(isnumeric(seq) && isreal(seq) && isvector(seq) && ~isempty(seq))
    error('Nelson:statistics:mustBeNonemptyVector', '%s must be a nonempty numeric vector.', name);
  end
  seq = double(seq(:)');
  if any(~isfinite(seq)) || any(floor(seq) ~= seq) || any(seq < 1) || any(seq > upperBound)
    error('Nelson:statistics:invalidIndex', '%s contains invalid indices.', name);
  end
end
%=============================================================================
