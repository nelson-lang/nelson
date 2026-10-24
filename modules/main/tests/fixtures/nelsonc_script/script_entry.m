%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear;
manifest = 42;
cleanup = 'application variable';
directory = 'application variable';
inputs = argv('user');
fprintf('SCRIPT_VALUE=%d\n', local_value(6));
fprintf('SCRIPT_ARGUMENTS=%s\n', jsonencode(inputs));
fid = fopen('script-output.txt', 'wt');
if fid < 0
  error('Nelson:nelsonc:fixture', 'Cannot create persistent script output.');
end
fprintf(fid, '%s', pwd());
fclose(fid);

function result = local_value(value)
  result = value * value;
end
