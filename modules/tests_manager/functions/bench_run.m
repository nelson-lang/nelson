%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function status = bench_run(varargin)
  targets = [];
  options = {};
  if nargin > 0
    targets = varargin{1};
    options = varargin(2:end);
  end
  results = nelson.unittest.run(targets, options{:}, 'Kind', 'bench');
  status = results.status;
end
%=============================================================================
