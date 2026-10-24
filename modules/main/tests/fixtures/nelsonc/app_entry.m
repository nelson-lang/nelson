%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

function app_entry(varargin)
  if ~isempty(varargin) && strcmp(varargin{1}, '--exit-seven')
    setenv('NELSONC_EXTRACTION_DIRECTORY', pwd());
    exit(7);
  end
  if ~isempty(varargin) && strcmp(varargin{1}, '--fail')
    error('Nelson:nelsonc:fixture', 'Deliberate application error.');
  end
  value = app_helper(4) + ncpkg.twice(3);
  fprintf('VALUE=%d\n', value);
  resourceDirectory = fileparts(mfilename('fullpath'));
  fprintf('RESOURCE=%s\n', fileread(fullfile(resourceDirectory, 'data.txt')));
  fprintf('INPUT=%s\n', fileread(fullfile(pwd(), 'invocation-input.txt')));
  fid = fopen('function-output.txt', 'wt');
  if fid < 0
    error('Nelson:nelsonc:fixture', 'Cannot create persistent function output.');
  end
  fprintf(fid, '%s', pwd());
  fclose(fid);
  fprintf('ARGUMENTS=%s\n', jsonencode(varargin));
end
