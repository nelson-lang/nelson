%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function package_entry(varargin)
  value = ncouter.ncinner.leaf(4);
  fprintf('PACKAGE_VALUE=%d\n', value);
  fprintf('PACKAGE_ARGUMENTS=%s\n', jsonencode(varargin));
  fid = fopen('package-output.txt', 'wt');
  if fid < 0
    error('Nelson:nelsonc:fixture', 'Cannot create persistent package output.');
  end
  fprintf(fid, '%s', pwd());
  fclose(fid);
end
