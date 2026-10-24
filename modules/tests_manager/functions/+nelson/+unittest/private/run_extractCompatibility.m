%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function [args, compatibility] = run_extractCompatibility(args)
  compatibility = false;
  if length(args) >= 2 && ischar(args{end - 1}) && strcmp(args{end - 1}, 'Compatibility')
    compatibility = logical(args{end});
    args(end - 1:end) = [];
  end
end
