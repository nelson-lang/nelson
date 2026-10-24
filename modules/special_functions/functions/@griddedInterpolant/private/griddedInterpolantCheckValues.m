%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function values = griddedInterpolantCheckValues(value)
  if ~isnumeric(value) || ~isreal(value) || issparse(value)
    error('Nelson:griddedInterpolant:invalidSampleValues', 'Sample values must be a real full numeric array.');
  end
  values = value;
end
%=============================================================================
