%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = arguments_validation_function(x, method)
  arguments
    x (1,:) double {mustBeNumeric, mustBeReal}
    method (1,:) char {mustBeMember(method, {'linear', 'nearest'})} = 'linear'
  end
  r = {x, method};
end
%=============================================================================
