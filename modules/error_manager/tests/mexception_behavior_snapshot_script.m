%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
isNelson = exist('nelsonroot', 'builtin') == 5;
if isNelson
  addpath([nelsonroot(), '/modules/error_manager/tests']);
  addpath([nelsonroot(), '/modules/error_manager/functions']);
else
  addpath(fileparts(mfilename('fullpath')));
end

mexceptionSnapshot = mexception_behavior_snapshot();

if exist('mexceptionSnapshotOutput', 'var')
  fid = fopen(mexceptionSnapshotOutput, 'wt');
  fprintf(fid, '%s', jsonencode(mexceptionSnapshot));
  fclose(fid);
else
  mexceptionSnapshot
end
%=============================================================================
