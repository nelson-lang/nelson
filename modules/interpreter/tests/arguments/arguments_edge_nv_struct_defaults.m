%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = arguments_edge_nv_struct_defaults(opts)
  arguments
    opts.Method (1,:) char {mustBeMember(opts.Method, {'linear', 'nearest'})} = 'linear'
    opts.Scale double {mustBePositive} = 2
    opts.Offset double = opts.Scale + 3
  end
  out = {opts.Method, opts.Scale, opts.Offset};
end
%=============================================================================
