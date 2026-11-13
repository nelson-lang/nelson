%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [encoding, options] = fopenOptions(defaults, varargin)
  % fopen-like arguments: optional encoding first, then name-value options.
  encoding = 'UTF-8';
  if mod(numel(varargin), 2) == 1
    encoding = char(varargin{1});
    varargin(1) = [];
  end
  options = nelson.io.internal.nameValueOptions(defaults, varargin{:});
end
%=============================================================================
