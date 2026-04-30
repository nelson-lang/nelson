%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = arguments_edge_nv_cross_reference(varargin)
  arguments
    varargin.scale double = 2
    varargin.offset double = varargin.scale
  end
  out = varargin.offset;
end
%=============================================================================
